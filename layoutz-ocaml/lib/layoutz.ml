(*
 * +==========================================================================+
 * |                               layoutz                                    |
 * |               Friendly, expressive print-layout DSL                      |
 * |                           Version 0.0.1                                  |
 * |                                                                          |
 * | Copyright 2025 Matthieu Court (matthieu.court@protonmail.com)            |
 * | Apache License 2.0                                                       |
 * +==========================================================================+
 *)

(* ============================================================================
   Core Element Interface
   ============================================================================ *)

(** The ELEMENT signature - implement this to create custom elements. *)
module type ELEMENT = sig
  type t

  val render : t -> string
  val width : t -> int
  val height : t -> int
end

(** Border styles for borderable elements *)
module Border = struct
  type t =
    | Normal
    | Double
    | Thick
    | Round
    | None

  let chars = function
    | Normal -> ("┌", "┐", "└", "┘", "─", "│")
    | Double -> ("╔", "╗", "╚", "╝", "═", "║")
    | Thick -> ("┏", "┓", "┗", "┛", "━", "┃")
    | Round -> ("╭", "╮", "╰", "╯", "─", "│")
    | None -> (" ", " ", " ", " ", " ", " ")
end

type border = Border.t

(** The BORDERABLE signature - elements that have configurable borders *)
module type BORDERABLE = sig
  type t

  val render : t -> string
  val width : t -> int
  val height : t -> int
  val with_border : t -> Border.t -> t
  val get_border : t -> Border.t
end

(** Core element type *)
type element =
  | E : (module ELEMENT with type t = 'a) * 'a -> element
  | B : (module BORDERABLE with type t = 'a) * 'a -> element
  | AutoCenter : element -> element
  | Bordered : Border.t * element -> element

(* ============================================================================
   Utility Functions
   ============================================================================ *)

(** Strip ANSI escape codes from a string for accurate width calculation *)
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let rec loop i in_escape =
    if i >= String.length s
    then Buffer.contents buf
    else
      let c = s.[i] in
      if in_escape
      then if c = 'm' then loop (i + 1) false else loop (i + 1) true
      else if c = '\027' && i + 1 < String.length s && s.[i + 1] = '['
      then loop (i + 2) true
      else begin
        Buffer.add_char buf c;
        loop (i + 1) false
      end
  in
  loop 0 false

(** Count UTF-8 characters in a string *)
let utf8_length s =
  let rec count i acc =
    if i >= String.length s
    then acc
    else
      let c = Char.code s.[i] in
      if c land 0xC0 = 0x80 then count (i + 1) acc else count (i + 1) (acc + 1)
  in
  count 0 0

(** Calculate visible length of a string (ignoring ANSI codes, counting UTF-8
    chars) *)
let visible_length s = utf8_length (strip_ansi s)

(** Split a string into lines *)
let lines s =
  let rec aux acc start i =
    if i >= String.length s
    then
      let last = String.sub s start (i - start) in
      List.rev (last :: acc)
    else if s.[i] = '\n'
    then
      let line = String.sub s start (i - start) in
      aux (line :: acc) (i + 1) (i + 1)
    else aux acc start (i + 1)
  in
  if s = "" then [ "" ] else aux [] 0 0

(** Join lines with newlines *)
let unlines ls = String.concat "\n" ls

(** Pad a string to target width on the right *)
let pad_right target_width s =
  let len = visible_length s in
  if len >= target_width then s else s ^ String.make (target_width - len) ' '

(** Pad a string to target width on the left *)
let pad_left target_width s =
  let len = visible_length s in
  if len >= target_width then s else String.make (target_width - len) ' ' ^ s

(** Center a string within target width *)
let center_string target_width s =
  let len = visible_length s in
  if len >= target_width
  then s
  else
    let total_pad = target_width - len in
    let left_pad = total_pad / 2 in
    let right_pad = total_pad - left_pad in
    String.make left_pad ' ' ^ s ^ String.make right_pad ' '

(** Repeat a UTF-8 string n times *)
let repeat_str n s = String.concat "" (List.init n (fun _ -> s))

(* ============================================================================
   Core Operations on Elements
   ============================================================================ *)

(* Forward reference for bordered wrapper rendering *)
let render_bordered_impl = ref (fun (_ : Border.t) (_ : string) -> "")

let rec render_el = function
  | E ((module M), v) -> M.render v
  | B ((module M), v) -> M.render v
  | AutoCenter inner -> render_el inner
  | Bordered (border, inner) -> !render_bordered_impl border (render_el inner)

let rec width_el = function
  | E ((module M), v) -> M.width v
  | B ((module M), v) -> M.width v
  | AutoCenter inner -> width_el inner
  | Bordered (_, inner) -> width_el inner + 4

let rec height_el = function
  | E ((module M), v) -> M.height v
  | B ((module M), v) -> M.height v
  | AutoCenter inner -> height_el inner
  | Bordered (_, inner) -> height_el inner + 2

(** Wrap any ELEMENT module + value into the universal element type *)
let el (type a) (module M : ELEMENT with type t = a) (v : a) : element =
  E ((module M), v)

(** Wrap any BORDERABLE module + value into the universal element type *)
let bel (type a) (module M : BORDERABLE with type t = a) (v : a) : element =
  B ((module M), v)

(** Set border on any element - borderables get modified, others get wrapped *)
let set_border border = function
  | B ((module M), v) -> B ((module M), M.with_border v border)
  | Bordered (_, inner) -> Bordered (border, inner)
  | other -> Bordered (border, other)

let borderNormal e = set_border Border.Normal e
let borderDouble e = set_border Border.Double e
let borderThick e = set_border Border.Thick e
let borderRound e = set_border Border.Round e

let borderNone = function
  | B ((module M), v) -> B ((module M), M.with_border v Border.None)
  | Bordered (_, inner) -> inner
  | other -> other

(* ============================================================================
   ANSI Colors and Styles
   ============================================================================ *)

module Color = struct
  type t =
    | None
    | Code of int
    | C256 of int
    | RGB of int * int * int

  let fg_code = function
    | None -> ""
    | Code n -> string_of_int n
    | C256 n -> "38;5;" ^ string_of_int n
    | RGB (r, g, b) ->
      "38;2;" ^ string_of_int r ^ ";" ^ string_of_int g ^ ";" ^ string_of_int b

  let bg_code = function
    | None -> ""
    | Code n -> string_of_int (n + 10)
    | C256 n -> "48;5;" ^ string_of_int n
    | RGB (r, g, b) ->
      "48;2;" ^ string_of_int r ^ ";" ^ string_of_int g ^ ";" ^ string_of_int b

  (* Raw color values for use with marginColor, underlineColored, etc. *)
  let black = Code 30
  let red = Code 31
  let green = Code 32
  let yellow = Code 33
  let blue = Code 34
  let magenta = Code 35
  let cyan = Code 36
  let white = Code 37
  let brightBlack = Code 90
  let brightRed = Code 91
  let brightGreen = Code 92
  let brightYellow = Code 93
  let brightBlue = Code 94
  let brightMagenta = Code 95
  let brightCyan = Code 96
  let brightWhite = Code 97
  let rgb r g b = RGB (r, g, b)
  let c256 n = C256 n
end

(* Color values *)
type color = Color.t

module Style = struct
  type t = string list
  (** A style is a list of ANSI codes that can be composed *)

  let none : t = []
  let codes (style : t) : string list = style

  (* Style codes *)
  let bold : t = [ "1" ]
  let dim : t = [ "2" ]
  let italic : t = [ "3" ]
  let underline : t = [ "4" ]
  let blink : t = [ "5" ]
  let reverse : t = [ "7" ]
  let hidden : t = [ "8" ]
  let strikethrough : t = [ "9" ]

  (** Compose two styles together *)
  let compose (a : t) (b : t) : t = a @ b
end

(** Compose two element transformers with ++ *)
let ( ++ ) f g x = g (f x)

(** Styled element wrapper *)
module Styled = struct
  type t = {
    fg: Color.t;
    bg: Color.t;
    style: Style.t;
    inner: element;
  }

  let create ?(fg = Color.None) ?(bg = Color.None) ?(style = Style.none) inner =
    { fg; bg; style; inner }

  let render t =
    let codes =
      (if t.fg = Color.None then [] else [ Color.fg_code t.fg ])
      @ (if t.bg = Color.None then [] else [ Color.bg_code t.bg ])
      @ Style.codes t.style
    in
    if codes = []
    then render_el t.inner
    else
      let start_code = "\027[" ^ String.concat ";" codes ^ "m" in
      let end_code = "\027[0m" in
      (* Apply to each line to handle multiline elements *)
      let content = render_el t.inner in
      let ls = lines content in
      unlines (List.map (fun line -> start_code ^ line ^ end_code) ls)

  let width t = width_el t.inner
  let height t = height_el t.inner
end

(* ============================================================================
   Built-in Elements
   ============================================================================ *)

(** Text element *)
module Text = struct
  type t = string

  let create s = s
  let render t = t

  let width t =
    let ls = lines t in
    List.fold_left (fun acc l -> max acc (visible_length l)) 0 ls

  let height t = List.length (lines t)
end

(** Line break element *)
module LineBreak = struct
  type t = int (* number of line breaks *)

  let create ?(n = 1) () = n
  let render t = String.concat "" (List.init (t - 1) (fun _ -> "\n"))
  let width _ = 0
  let height t = t
end

(** Space element *)
module Space = struct
  type t = int

  let create ?(n = 1) () = n
  let render t = String.make t ' '
  let width t = t
  let height _ = 1
end

(** Empty element *)
module Empty = struct
  type t = unit

  let create () = ()
  let render _ = ""
  let width _ = 0
  let height _ = 0
end

(** Horizontal rule *)
module HorizontalRule = struct
  type t = {
    char: string;
    rule_width: int;
  }

  let create ?(char = "─") ?(rule_width = 50) () = { char; rule_width }

  let render t =
    let char_vis_len = visible_length t.char in
    if char_vis_len = 0
    then String.make t.rule_width ' '
    else
      let repeats = t.rule_width / char_vis_len in
      String.concat "" (List.init repeats (fun _ -> t.char))

  let width t = t.rule_width
  let height _ = 1
end

(** Vertical rule *)
module VerticalRule = struct
  type t = {
    char: string;
    rule_height: int;
  }

  let create ?(char = "│") ?(rule_height = 10) () = { char; rule_height }
  let render t = String.concat "\n" (List.init t.rule_height (fun _ -> t.char))
  let width t = visible_length t.char
  let height t = t.rule_height
end

(** Centered element *)
module Centered = struct
  type t = {
    inner: element;
    target_width: int;
  }

  let create ~width inner = { inner; target_width = width }

  let render t =
    let content = render_el t.inner in
    let ls = lines content in
    unlines (List.map (center_string t.target_width) ls)

  let width t = t.target_width
  let height t = height_el t.inner
end

(** Left-aligned element *)
module LeftAligned = struct
  type t = {
    inner: element;
    target_width: int;
  }

  let create ~width inner = { inner; target_width = width }

  let render t =
    let content = render_el t.inner in
    let ls = lines content in
    unlines (List.map (pad_right t.target_width) ls)

  let width t = t.target_width
  let height t = height_el t.inner
end

(** Right-aligned element *)
module RightAligned = struct
  type t = {
    inner: element;
    target_width: int;
  }

  let create ~width inner = { inner; target_width = width }

  let render t =
    let content = render_el t.inner in
    let ls = lines content in
    unlines (List.map (pad_left t.target_width) ls)

  let width t = t.target_width
  let height t = height_el t.inner
end

(** Truncated element *)
module Truncated = struct
  type t = {
    inner: element;
    max_width: int;
    ellipsis: string;
  }

  let create ~max_width ?(ellipsis = "...") inner =
    { inner; max_width; ellipsis }

  let truncate_line max_w ellipsis line =
    let len = visible_length line in
    if len <= max_w
    then line
    else
      let ellipsis_len = visible_length ellipsis in
      let target = max_w - ellipsis_len in
      if target <= 0
      then String.sub ellipsis 0 (min max_w (String.length ellipsis))
      else
        (* Byte truncation - TODO check if this works for all UTF-8, ASCII should be fine *)
        let rec find_cut i vis =
          if i >= String.length line || vis >= target
          then i
          else
            let c = Char.code line.[i] in
            if c < 128
            then find_cut (i + 1) (vis + 1)
            else if c < 224
            then find_cut (i + 2) (vis + 1)
            else if c < 240
            then find_cut (i + 3) (vis + 1)
            else find_cut (i + 4) (vis + 1)
        in
        let cut = find_cut 0 0 in
        String.sub line 0 cut ^ ellipsis

  let render t =
    let content = render_el t.inner in
    let ls = lines content in
    unlines (List.map (truncate_line t.max_width t.ellipsis) ls)

  let width t = min t.max_width (width_el t.inner)
  let height t = height_el t.inner
end

(** Wrapped element - word wrap *)
module Wrapped = struct
  type t = {
    inner: element;
    max_width: int;
  }

  let create ~max_width inner = { inner; max_width }

  let wrap_line max_w line =
    let words = String.split_on_char ' ' line in
    let rec build_lines current_line current_len = function
      | [] -> [ current_line ]
      | word :: rest ->
        let word_len = visible_length word in
        if current_line = ""
        then build_lines word word_len rest
        else if current_len + 1 + word_len <= max_w
        then
          build_lines
            (current_line ^ " " ^ word)
            (current_len + 1 + word_len)
            rest
        else current_line :: build_lines word word_len rest
    in
    build_lines "" 0 words

  let render t =
    let content = render_el t.inner in
    let ls = lines content in
    let wrapped = List.concat_map (wrap_line t.max_width) ls in
    unlines wrapped

  let width t = t.max_width

  let height t =
    let content = render_el t.inner in
    let ls = lines content in
    List.length (List.concat_map (wrap_line t.max_width) ls)
end

(** Justified element *)
module Justified = struct
  type t = {
    inner: element;
    target_width: int;
    justify_last: bool;
  }

  let create ~target_width ?(justify_last = false) inner =
    { inner; target_width; justify_last }

  let justify_line width line =
    let words = String.split_on_char ' ' line in
    let words = List.filter (fun w -> String.length w > 0) words in
    match words with
    | [] -> String.make width ' '
    | [ word ] -> pad_right width word
    | _ ->
      let total_word_len =
        List.fold_left (fun acc w -> acc + visible_length w) 0 words
      in
      let gaps = List.length words - 1 in
      let total_space = width - total_word_len in
      if total_space <= 0 || gaps <= 0
      then String.concat " " words
      else
        let base_space = total_space / gaps in
        let extra = total_space mod gaps in
        let rec build i = function
          | [] -> ""
          | [ w ] -> w
          | w :: rest ->
            let sp = base_space + if i < extra then 1 else 0 in
            w ^ String.make sp ' ' ^ build (i + 1) rest
        in
        build 0 words

  let render t =
    let content = render_el t.inner in
    let ls = lines content in
    let n = List.length ls in
    let justify_line_at i line =
      if i = n - 1 && not t.justify_last
      then pad_right t.target_width line
      else justify_line t.target_width line
    in
    unlines (List.mapi justify_line_at ls)

  let width t = t.target_width
  let height t = height_el t.inner
end

(** Padded element *)
module Padded = struct
  type t = {
    inner: element;
    padding: int;
  }

  let create ~padding inner = { inner; padding }

  let render t =
    let content = render_el t.inner in
    let content_lines = lines content in
    let max_width =
      List.fold_left (fun acc l -> max acc (visible_length l)) 0 content_lines
    in
    let h_pad = String.make t.padding ' ' in
    let total_width = max_width + (t.padding * 2) in
    let v_pad = String.make total_width ' ' in
    let padded_lines =
      List.map
        (fun line -> h_pad ^ pad_right max_width line ^ h_pad)
        content_lines
    in
    let v_lines = List.init t.padding (fun _ -> v_pad) in
    unlines (v_lines @ padded_lines @ v_lines)

  let width t = width_el t.inner + (t.padding * 2)
  let height t = height_el t.inner + (t.padding * 2)
end

(** Margin - prefix each line *)
module Margin = struct
  type t = {
    prefix: string;
    color: Color.t option;
    inner: element;
  }

  let create ~prefix ?color inner = { prefix; color; inner }

  let render t =
    let content = render_el t.inner in
    let ls = lines content in
    let styled_prefix =
      match t.color with
      | None -> t.prefix
      | Some c -> "\027[" ^ Color.fg_code c ^ "m" ^ t.prefix ^ "\027[0m"
    in
    unlines (List.map (fun l -> styled_prefix ^ " " ^ l) ls)

  let width t = visible_length t.prefix + 1 + width_el t.inner
  let height t = height_el t.inner
end

(** Underline - add underline below element *)
module Underline = struct
  type t = {
    inner: element;
    char: string;
    color: Color.t option;
  }

  let create ?(char = "─") ?color inner = { inner; char; color }

  let render t =
    let content = render_el t.inner in
    let w = width_el t.inner in
    let underline_str = repeat_str w t.char in
    let styled_underline =
      match t.color with
      | None -> underline_str
      | Some c -> "\027[" ^ Color.fg_code c ^ "m" ^ underline_str ^ "\027[0m"
    in
    content ^ "\n" ^ styled_underline

  let width t = width_el t.inner
  let height t = height_el t.inner + 1
end

(** Columns - multi-column layout *)
module Columns = struct
  type t = {
    elements: element list;
    spacing: int;
  }

  let create ?(spacing = 2) elements = { elements; spacing }

  let render t =
    if t.elements = []
    then ""
    else
      let rendered = List.map render_el t.elements in
      let element_lines = List.map lines rendered in
      let max_height =
        List.fold_left (fun acc ls -> max acc (List.length ls)) 0 element_lines
      in
      let widths =
        List.map
          (fun ls ->
            List.fold_left (fun acc l -> max acc (visible_length l)) 0 ls)
          element_lines
      in
      let sep = String.make t.spacing ' ' in
      let padded =
        List.map2
          (fun w ls ->
            let padded_lines = List.map (pad_right w) ls in
            let missing = max_height - List.length ls in
            padded_lines @ List.init missing (fun _ -> String.make w ' '))
          widths element_lines
      in
      let transposed =
        List.init max_height (fun row ->
            String.concat sep (List.map (fun col -> List.nth col row) padded))
      in
      unlines transposed

  let width t =
    let widths = List.map width_el t.elements in
    let total = List.fold_left ( + ) 0 widths in
    let gaps = t.spacing * max 0 (List.length t.elements - 1) in
    total + gaps

  let height t =
    List.fold_left (fun acc e -> max acc (height_el e)) 0 t.elements
end

(** StatusCard - label with content *)
module StatusCard = struct
  type t = {
    label: element;
    content: element;
  }

  let create ~label ~content = { label; content }

  let render t =
    let label_str = render_el t.label in
    let content_str = render_el t.content in
    label_str ^ ": " ^ content_str

  let width t = width_el t.label + 2 + width_el t.content
  let height _ = 1
end

(** Key-value pairs *)
module KeyValue = struct
  type t = (string * string) list

  let create pairs = pairs

  let render t =
    if t = []
    then ""
    else
      let max_key_len =
        List.fold_left (fun acc (k, _) -> max acc (visible_length k)) 0 t
      in
      let align_pos = max_key_len + 2 in
      let render_pair (key, value) =
        let key_with_colon = key ^ ":" in
        let spaces = max 1 (align_pos - visible_length key_with_colon) in
        key_with_colon ^ String.make spaces ' ' ^ value
      in
      unlines (List.map render_pair t)

  let width t =
    if t = []
    then 0
    else
      let max_key_len =
        List.fold_left (fun acc (k, _) -> max acc (visible_length k)) 0 t
      in
      let max_val_len =
        List.fold_left (fun acc (_, v) -> max acc (visible_length v)) 0 t
      in
      max_key_len + 2 + max_val_len

  let height t = List.length t
end

(** Inline progress bar *)
module InlineBar = struct
  type t = {
    label: string;
    progress: float;
  }

  let create ~label ~progress = { label; progress = max 0.0 (min 1.0 progress) }

  let render t =
    let bar_width = 20 in
    let filled = int_of_float (t.progress *. float_of_int bar_width) in
    let empty = bar_width - filled in
    let bar = repeat_str filled "█" ^ repeat_str empty "─" in
    let pct = int_of_float (t.progress *. 100.0) in
    Printf.sprintf "%s [%s] %d%%" t.label bar pct

  let width t = visible_length t.label + 3 + 20 + 5
  let height _ = 1
end

(** Chart - horizontal bar chart *)
module Chart = struct
  type t = (string * float) list

  let create data = data

  let render t =
    if t = []
    then "No data"
    else
      let max_value = List.fold_left (fun acc (_, v) -> max acc v) 0.0 t in
      let max_label_width =
        min 15
          (List.fold_left (fun acc (l, _) -> max acc (String.length l)) 0 t)
      in
      let chart_width = 40 in
      let render_bar (label, value) =
        let truncated_label =
          if String.length label > max_label_width
          then String.sub label 0 (max_label_width - 3) ^ "..."
          else label
        in
        let padded_label = pad_right max_label_width truncated_label in
        let pct = if max_value = 0.0 then 0.0 else value /. max_value in
        let bar_len = int_of_float (pct *. float_of_int chart_width) in
        let bar =
          repeat_str bar_len "█" ^ repeat_str (chart_width - bar_len) "─"
        in
        let value_str =
          if value = floor value
          then string_of_int (int_of_float value)
          else Printf.sprintf "%.1f" value
        in
        padded_label ^ " │" ^ bar ^ "│ " ^ value_str
      in
      unlines (List.map render_bar t)

  let width _ = 15 + 3 + 40 + 10
  let height t = max 1 (List.length t)
end

(** Section with decorative header *)
module Section = struct
  type t = {
    title: string;
    glyph: string;
    flanking: int;
    content: element list;
  }

  let create ?(glyph = "=") ?(flanking = 3) ~title content =
    { title; glyph; flanking; content }

  let render t =
    let header =
      repeat_str t.flanking t.glyph
      ^ " "
      ^ t.title
      ^ " "
      ^ repeat_str t.flanking t.glyph
    in
    let body = String.concat "\n" (List.map render_el t.content) in
    header ^ "\n" ^ body

  let width t =
    let header_width = (t.flanking * 2) + 2 + visible_length t.title in
    let content_width =
      List.fold_left (fun acc e -> max acc (width_el e)) 0 t.content
    in
    max header_width content_width

  let height t = 1 + List.fold_left (fun acc e -> acc + height_el e) 0 t.content
end

(** VStack - vertical stacking with auto-center resolution *)
module VStack = struct
  type t = element list

  let create elements = elements

  let render t =
    (* Calculate layout width from all elements *)
    let layout_width = List.fold_left (fun acc e -> max acc (width_el e)) 0 t in
    (* Resolve AutoCenter elements *)
    let resolve_element e =
      match e with
      | AutoCenter inner ->
        let content = render_el inner in
        let ls = lines content in
        unlines (List.map (center_string layout_width) ls)
      | _ -> render_el e
    in
    String.concat "\n" (List.map resolve_element t)

  let width t = List.fold_left (fun acc e -> max acc (width_el e)) 0 t
  let height t = List.fold_left (fun acc e -> acc + height_el e) 0 t
end

(** HStack - horizontal arrangement *)
module HStack = struct
  type t = {
    elements: element list;
    tight: bool;
  }

  let create ?(tight = false) elements = { elements; tight }

  let render t =
    if t.elements = []
    then ""
    else
      let rendered = List.map render_el t.elements in
      let element_lines = List.map lines rendered in
      let max_height =
        List.fold_left (fun acc ls -> max acc (List.length ls)) 0 element_lines
      in
      let widths =
        List.map
          (fun ls ->
            List.fold_left (fun acc l -> max acc (visible_length l)) 0 ls)
          element_lines
      in
      let separator = if t.tight then "" else " " in
      let padded =
        List.map2
          (fun w ls ->
            let padded_lines = List.map (pad_right w) ls in
            let missing = max_height - List.length ls in
            padded_lines @ List.init missing (fun _ -> String.make w ' '))
          widths element_lines
      in
      let transposed =
        List.init max_height (fun row ->
            String.concat separator
              (List.map (fun col -> List.nth col row) padded))
      in
      unlines transposed

  let width t =
    let widths = List.map width_el t.elements in
    let total = List.fold_left ( + ) 0 widths in
    let gaps = if t.tight then 0 else max 0 (List.length t.elements - 1) in
    total + gaps

  let height t =
    List.fold_left (fun acc e -> max acc (height_el e)) 0 t.elements
end

(* ============================================================================
   Borderable Elements (Box, Table, Banner)
   ============================================================================ *)

(** Banner - decorative banner with border (implements BORDERABLE) *)
module Banner = struct
  type t = {
    content: element;
    border: Border.t;
  }

  let create ?(border = Border.Double) content = { content; border }
  let with_border t b = { t with border = b }
  let get_border t = t.border

  let render t =
    let content_str = render_el t.content in
    let content_lines = lines content_str in
    let max_width =
      List.fold_left (fun acc l -> max acc (visible_length l)) 0 content_lines
    in
    let inner_width = max_width + 4 in
    let tl, tr, bl, br, h, v = Border.chars t.border in
    let top = tl ^ repeat_str inner_width h ^ tr in
    let bot = bl ^ repeat_str inner_width h ^ br in
    let empty_line = v ^ String.make inner_width ' ' ^ v in
    let content_rendered =
      List.map
        (fun l -> v ^ "  " ^ pad_right max_width l ^ "  " ^ v)
        content_lines
    in
    unlines ([ top; empty_line ] @ content_rendered @ [ empty_line; bot ])

  let width t = width_el t.content + 8
  let height t = height_el t.content + 4
end

(** Box - bordered container (implements BORDERABLE) *)
module Box = struct
  type t = {
    title: string;
    elements: element list;
    border: Border.t;
  }

  let create ?(border = Border.Normal) ~title elements =
    { title; elements; border }

  let with_border t b = { t with border = b }
  let get_border t = t.border

  let render t =
    let content = String.concat "\n" (List.map render_el t.elements) in
    let content_lines = if content = "" then [ "" ] else lines content in
    let content_width =
      List.fold_left (fun acc l -> max acc (visible_length l)) 0 content_lines
    in
    let title_width = if t.title = "" then 0 else visible_length t.title + 2 in
    let inner_width = max content_width title_width in
    let total_width = inner_width + 4 in
    let tl, tr, bl, br, h, v = Border.chars t.border in
    let top_border =
      if t.title = ""
      then tl ^ repeat_str (total_width - 2) h ^ tr
      else
        let title_padding = total_width - visible_length t.title - 2 in
        let left_pad = title_padding / 2 in
        let right_pad = title_padding - left_pad in
        tl ^ repeat_str left_pad h ^ t.title ^ repeat_str right_pad h ^ tr
    in
    let bottom_border = bl ^ repeat_str (total_width - 2) h ^ br in
    let padded_content =
      List.map
        (fun line -> v ^ " " ^ pad_right inner_width line ^ " " ^ v)
        content_lines
    in
    unlines ((top_border :: padded_content) @ [ bottom_border ])

  let width t =
    let content = String.concat "\n" (List.map render_el t.elements) in
    let content_lines = if content = "" then [ "" ] else lines content in
    let content_width =
      List.fold_left (fun acc l -> max acc (visible_length l)) 0 content_lines
    in
    let title_width = if t.title = "" then 0 else visible_length t.title + 2 in
    max content_width title_width + 4

  let height t =
    let content = String.concat "\n" (List.map render_el t.elements) in
    let content_lines = if content = "" then [ "" ] else lines content in
    List.length content_lines + 2
end

(** Table - bordered table with element cells (implements BORDERABLE) *)
module Table = struct
  type t = {
    headers: element list;
    rows: element list list;
    border: Border.t;
  }

  let create ?(border = Border.Normal) ~headers rows = { headers; rows; border }
  let with_border t b = { t with border = b }
  let get_border t = t.border

  let render t =
    let num_cols = List.length t.headers in
    let normalize_row row =
      let len = List.length row in
      if len >= num_cols
      then List.filteri (fun i _ -> i < num_cols) row
      else row @ List.init (num_cols - len) (fun _ -> el (module Text) "")
    in
    let header_strs = List.map render_el t.headers in
    let rendered_rows =
      List.map (fun row -> List.map render_el (normalize_row row)) t.rows
    in
    let col_widths =
      let header_widths = List.map visible_length header_strs in
      let row_widths =
        List.map
          (fun row ->
            List.map
              (fun cell ->
                List.fold_left
                  (fun acc l -> max acc (visible_length l))
                  0 (lines cell))
              row)
          rendered_rows
      in
      List.mapi
        (fun i hw ->
          List.fold_left (fun acc row -> max acc (List.nth row i)) hw row_widths)
        header_widths
    in
    let tl, tr, bl, br, h, v = Border.chars t.border in
    let top_conn =
      match t.border with
      | Border.Round | Border.Normal -> "┬"
      | Border.Double -> "╦"
      | Border.Thick -> "┳"
      | Border.None -> " "
    in
    let mid_conn =
      match t.border with
      | Border.Round | Border.Normal -> "┼"
      | Border.Double -> "╬"
      | Border.Thick -> "╋"
      | Border.None -> " "
    in
    let bot_conn =
      match t.border with
      | Border.Round | Border.Normal -> "┴"
      | Border.Double -> "╩"
      | Border.Thick -> "┻"
      | Border.None -> " "
    in
    let left_tee =
      match t.border with
      | Border.Round | Border.Normal -> "├"
      | Border.Double -> "╠"
      | Border.Thick -> "┣"
      | Border.None -> " "
    in
    let right_tee =
      match t.border with
      | Border.Round | Border.Normal -> "┤"
      | Border.Double -> "╣"
      | Border.Thick -> "┫"
      | Border.None -> " "
    in
    let make_border left conn right =
      let parts = List.map (fun w -> repeat_str w h) col_widths in
      left ^ h ^ String.concat (h ^ conn ^ h) parts ^ h ^ right
    in
    let top_border = make_border tl top_conn tr in
    let sep_border = make_border left_tee mid_conn right_tee in
    let bot_border = make_border bl bot_conn br in
    let make_row cells =
      let padded = List.map2 pad_right col_widths cells in
      v ^ " " ^ String.concat (" " ^ v ^ " ") padded ^ " " ^ v
    in
    let header_row = make_row header_strs in
    let data_rows =
      List.map
        (fun row ->
          let cell_lines = List.map lines row in
          let max_h =
            List.fold_left (fun acc ls -> max acc (List.length ls)) 1 cell_lines
          in
          let padded_cells =
            List.map2
              (fun w cls ->
                let padded = List.map (pad_right w) cls in
                padded
                @ List.init
                    (max_h - List.length cls)
                    (fun _ -> String.make w ' '))
              col_widths cell_lines
          in
          let transposed =
            List.init max_h (fun r ->
                List.map (fun col -> List.nth col r) padded_cells)
          in
          List.map
            (fun row_cells ->
              v ^ " " ^ String.concat (" " ^ v ^ " ") row_cells ^ " " ^ v)
            transposed)
        rendered_rows
    in
    unlines
      ([ top_border; header_row; sep_border ]
      @ List.concat data_rows
      @ [ bot_border ])

  let width t =
    (* Must match render's column width calculation exactly *)
    let num_cols = List.length t.headers in
    let normalize_row row =
      let len = List.length row in
      if len >= num_cols
      then List.filteri (fun i _ -> i < num_cols) row
      else row @ List.init (num_cols - len) (fun _ -> el (module Text) "")
    in
    let header_strs = List.map render_el t.headers in
    let rendered_rows =
      List.map (fun row -> List.map render_el (normalize_row row)) t.rows
    in
    let col_widths =
      let header_widths = List.map visible_length header_strs in
      let row_widths =
        List.map
          (fun row ->
            List.map
              (fun cell ->
                List.fold_left
                  (fun acc l -> max acc (visible_length l))
                  0 (lines cell))
              row)
          rendered_rows
      in
      List.mapi
        (fun i hw ->
          List.fold_left (fun acc row -> max acc (List.nth row i)) hw row_widths)
        header_widths
    in
    let total = List.fold_left ( + ) 0 col_widths in
    total + (List.length col_widths * 3) + 1

  let height t =
    let row_heights =
      List.map
        (fun row ->
          List.fold_left (fun acc cell -> max acc (height_el cell)) 1 row)
        t.rows
    in
    3 + List.fold_left ( + ) 0 row_heights
end

(* Wire up the forward-declared bordered wrapper implementation *)
let () =
  render_bordered_impl :=
    fun border content_str ->
      let content_lines = lines content_str in
      let max_width =
        List.fold_left (fun acc l -> max acc (visible_length l)) 0 content_lines
      in
      let tl, tr, bl, br, h, v = Border.chars border in
      let top = tl ^ repeat_str (max_width + 2) h ^ tr in
      let bot = bl ^ repeat_str (max_width + 2) h ^ br in
      let body =
        List.map
          (fun line -> v ^ " " ^ pad_right max_width line ^ " " ^ v)
          content_lines
      in
      unlines ([ top ] @ body @ [ bot ])

type list_item = {
  content: element;
  children: list_item list;
}
(** List item type - recursive for deep nesting *)

type tree_node = {
  label: element;
  branches: tree_node list;
}
(** Tree node type - recursive *)

(** Unordered list - items are (element, children) pairs *)
module UList = struct
  type t = {
    items: list_item list;
    bullet: string;
  }

  let create ?(bullet = "•") items = { items; bullet }

  let rec render_with_indent indent level t =
    let prefix_indent = String.make indent ' ' in
    let bullet = t.bullet in
    let _ = level in
    (* same bullet at all levels *)
    let bullet_width = visible_length bullet + 1 in
    let format_item item =
      let rendered = render_el item.content in
      let item_lines = lines rendered in
      let first_part =
        match item_lines with
        | [] -> prefix_indent ^ bullet ^ " "
        | first :: rest ->
          let first_line = prefix_indent ^ bullet ^ " " ^ first in
          let cont_indent = String.make (indent + bullet_width) ' ' in
          let rest_lines = List.map (fun l -> cont_indent ^ l) rest in
          unlines (first_line :: rest_lines)
      in
      if item.children = []
      then first_part
      else
        let child_list = { items = item.children; bullet = t.bullet } in
        let child_rendered =
          render_with_indent (indent + 2) (level + 1) child_list
        in
        first_part ^ "\n" ^ child_rendered
    in
    String.concat "\n" (List.map format_item t.items)

  let render t = render_with_indent 0 0 t

  let width t =
    let bullet_width = visible_length t.bullet + 1 in
    let max_item_width =
      List.fold_left (fun acc item -> max acc (width_el item.content)) 0 t.items
    in
    bullet_width + max_item_width

  let height t =
    List.fold_left
      (fun acc item -> acc + height_el item.content + List.length item.children)
      0 t.items
end

(** Ordered list - items are (element, children) pairs *)
module OList = struct
  type t = {
    items: list_item list;
    start: int;
  }

  (* Convert number to letter (a, b, c, ..., z, aa, ab, ...) *)
  let to_letter n =
    let rec aux acc n =
      if n < 0
      then acc
      else aux (String.make 1 (Char.chr (97 + (n mod 26))) ^ acc) ((n / 26) - 1)
    in
    aux "" n

  (* Convert number to roman numeral *)
  let to_roman n =
    let numerals =
      [
        (1000, "m");
        (900, "cm");
        (500, "d");
        (400, "cd");
        (100, "c");
        (90, "xc");
        (50, "l");
        (40, "xl");
        (10, "x");
        (9, "ix");
        (5, "v");
        (4, "iv");
        (1, "i");
      ]
    in
    let rec aux acc n = function
      | [] -> acc
      | (value, numeral) :: rest ->
        if n >= value
        then aux (acc ^ numeral) (n - value) ((value, numeral) :: rest)
        else aux acc n rest
    in
    if n <= 0 then string_of_int n else aux "" n numerals

  let create ?(start = 1) items = { items; start }

  let rec render_with_indent prefix_indent level start items =
    let format_num level idx =
      let n = start + idx in
      match level mod 3 with
      | 0 -> string_of_int n
      | 1 -> to_letter (n - 1)
      | _ -> to_roman n
    in
    let sample_nums = List.mapi (fun i _ -> format_num level i) items in
    let num_width =
      List.fold_left (fun acc s -> max acc (String.length s)) 0 sample_nums
    in
    let format_item idx item =
      let num_str = format_num level idx in
      let padded_num =
        String.make (num_width - String.length num_str) ' ' ^ num_str
      in
      let prefix = padded_num ^ ". " in
      let prefix_width = num_width + 2 in
      let rendered = render_el item.content in
      let item_lines = lines rendered in
      let indent_str = String.make prefix_indent ' ' in
      let first_part =
        match item_lines with
        | [] -> indent_str ^ prefix
        | first :: rest ->
          let first_line = indent_str ^ prefix ^ first in
          let cont_indent = String.make (prefix_indent + prefix_width) ' ' in
          let rest_lines = List.map (fun l -> cont_indent ^ l) rest in
          unlines (first_line :: rest_lines)
      in
      if item.children = []
      then first_part
      else
        let child_rendered =
          render_with_indent
            (prefix_indent + prefix_width)
            (level + 1) 1 item.children
        in
        first_part ^ "\n" ^ child_rendered
    in
    String.concat "\n" (List.mapi format_item items)

  let render t = render_with_indent 0 0 t.start t.items

  let width t =
    let num_items = List.length t.items in
    let max_num = t.start + num_items - 1 in
    let num_width = String.length (string_of_int max_num) + 2 in
    let max_item_width =
      List.fold_left (fun acc item -> max acc (width_el item.content)) 0 t.items
    in
    num_width + max_item_width

  let height t =
    List.fold_left
      (fun acc item -> acc + height_el item.content + List.length item.children)
      0 t.items
end

(** Tree - renders with box-drawing connectors *)
module Tree = struct
  type t = tree_node

  let create node = node

  let rec render_node prefix is_last node =
    let connector = if is_last then "└── " else "├── " in
    let rendered = render_el node.label in
    let label_lines = lines rendered in
    let first_line =
      prefix
      ^ connector
      ^
      match label_lines with
      | [] -> ""
      | h :: _ -> h
    in
    let child_prefix = prefix ^ if is_last then "    " else "│   " in
    let rest_lines =
      match label_lines with
      | [] | [ _ ] -> []
      | _ :: rest -> List.map (fun l -> child_prefix ^ l) rest
    in
    let num_children = List.length node.branches in
    let children_rendered =
      List.mapi
        (fun i child -> render_node child_prefix (i = num_children - 1) child)
        node.branches
    in
    String.concat "\n" ([ first_line ] @ rest_lines @ children_rendered)

  let render t =
    let rendered = render_el t.label in
    let label_lines = lines rendered in
    let root_line =
      match label_lines with
      | [] -> ""
      | h :: _ -> h
    in
    let rest_lines =
      match label_lines with
      | [] | [ _ ] -> []
      | _ :: rest -> rest
    in
    let num_children = List.length t.branches in
    let children_rendered =
      List.mapi
        (fun i child -> render_node "" (i = num_children - 1) child)
        t.branches
    in
    String.concat "\n" ([ root_line ] @ rest_lines @ children_rendered)

  let rec node_width node =
    let label_w = width_el node.label in
    let children_w =
      List.fold_left
        (fun acc child -> max acc (4 + node_width child))
        0 node.branches
    in
    max label_w children_w

  let width t = node_width t

  let rec node_height node =
    1
    + List.fold_left (fun acc child -> acc + node_height child) 0 node.branches

  let height t = node_height t
end

(* ============================================================================
   Smart Constructors
   ============================================================================ *)

(** String to element - the short way *)
let s str = el (module Text) (Text.create str)

(** String to element - verbose name *)
let text str = el (module Text) (Text.create str)

(** Line break *)
let br = el (module LineBreak) (LineBreak.create ())

(** Multiple line breaks *)
let br' n = el (module LineBreak) (LineBreak.create ~n ())

(** Single space *)
let space = el (module Space) (Space.create ())

(** Multiple spaces *)
let space' n = el (module Space) (Space.create ~n ())

(** Empty element *)
let empty = el (module Empty) (Empty.create ())

(** Horizontal rule (default: 50 wide with ─) *)
let hr = el (module HorizontalRule) (HorizontalRule.create ())

(** Horizontal rule with options *)
let hr' ?(char = "─") ?(width = 50) () =
  el (module HorizontalRule) (HorizontalRule.create ~char ~rule_width:width ())

(** Vertical rule (default: 10 high with │) *)
let vr ?(char = "│") ?(height = 10) () =
  el (module VerticalRule) (VerticalRule.create ~char ~rule_height:height ())

(** Center element - auto-centers in layout, or specify width *)
let center ?width inner =
  match width with
  | Some w -> el (module Centered) (Centered.create ~width:w inner)
  | None -> AutoCenter inner

(** Left-align element within width *)
let left_align ~width inner =
  el (module LeftAligned) (LeftAligned.create ~width inner)

(** Right-align element within width *)
let right_align ~width inner =
  el (module RightAligned) (RightAligned.create ~width inner)

(** Add padding around element *)
let pad ~padding inner = el (module Padded) (Padded.create ~padding inner)

(** Add prefix margin to element *)
let margin ~prefix inner = el (module Margin) (Margin.create ~prefix inner)

(** Add colored prefix margin to element *)
let marginColor ~prefix ~color inner =
  el (module Margin) (Margin.create ~prefix ~color inner)

(** Truncate element to max width *)
let truncate ~max_width ?ellipsis inner =
  match ellipsis with
  | Some e ->
    el (module Truncated) (Truncated.create ~max_width ~ellipsis:e inner)
  | None -> el (module Truncated) (Truncated.create ~max_width inner)

(** Word wrap element to max width *)
let wrap ~max_width inner =
  el (module Wrapped) (Wrapped.create ~max_width inner)

(** Justify text to width *)
let justify ~width inner =
  el (module Justified) (Justified.create ~target_width:width inner)

(** Justify all lines including last *)
let justifyAll ~width inner =
  el
    (module Justified)
    (Justified.create ~target_width:width ~justify_last:true inner)

(** Underline element *)
let underline ?char inner =
  match char with
  | Some c -> el (module Underline) (Underline.create ~char:c inner)
  | None -> el (module Underline) (Underline.create inner)

(** Colored underline *)
let underlineColored ~char ~color inner =
  el (module Underline) (Underline.create ~char ~color inner)

(** Multi-column layout *)
let columns ?spacing elements =
  match spacing with
  | Some sp -> el (module Columns) (Columns.create ~spacing:sp elements)
  | None -> el (module Columns) (Columns.create elements)

(** Status card - bordered box with label and content inside *)
let statusCard ~label ~content =
  bel
    (module Box)
    { Box.title = ""; elements = [ label; content ]; border = Border.Normal }

(** Banner with decorative border *)
let banner content =
  bel (module Banner) { Banner.content; border = Border.Double }

(** Key-value pairs *)
let kv pairs = el (module KeyValue) (KeyValue.create pairs)

(** Inline progress bar *)
let inline_bar ~label ~progress =
  el (module InlineBar) (InlineBar.create ~label ~progress)

(** Horizontal bar chart *)
let chart data = el (module Chart) (Chart.create data)

(** Section with title and content *)
let section ?(glyph = "=") ?(flanking = 3) ~title content =
  el (module Section) (Section.create ~glyph ~flanking ~title content)

(** Vertical layout *)
let layout elements = el (module VStack) (VStack.create elements)

(** Horizontal row *)
let row ?(tight = false) elements =
  el (module HStack) (HStack.create ~tight elements)

(** Tight row - horizontal arrangement without gaps *)
let tightRow elements = el (module HStack) (HStack.create ~tight:true elements)

(** Bordered box *)
let box ~title elements =
  bel (module Box) { Box.title; elements; border = Border.Normal }

(** Table with element headers and rows *)
let table ~headers rows =
  bel (module Table) { Table.headers; rows; border = Border.Normal }

(** List item - optionally with nested children via ~c *)
let li ?(c = []) item : list_item = { content = item; children = c }

(** Unordered list - takes elements or li items *)
let ul ?(bullet = "•") items = el (module UList) (UList.create ~bullet items)

(** Ordered list - takes elements or li items *)
let ol ?(start = 1) items = el (module OList) (OList.create ~start items)

(** Tree node constructor *)
let node ?(c = []) label : tree_node = { label; branches = c }

(** Tree element *)
let tree root = el (module Tree) (Tree.create root)

(** Styling functions - pipe-friendly (element last) *)
let withStyle ?(fg = Color.None) ?(bg = Color.None) ?(style = Style.none) inner
    =
  el (module Styled) (Styled.create ~fg ~bg ~style inner)

(** Apply foreground color (for use with raw Color.t values) *)
let fg color e = withStyle ~fg:color e

(** Apply background color (for use with raw Color.t values) *)
let bg color e = withStyle ~bg:color e

(** Foreground color functions - pipe-friendly *)
let colorBlack e = withStyle ~fg:Color.black e

let colorRed e = withStyle ~fg:Color.red e
let colorGreen e = withStyle ~fg:Color.green e
let colorYellow e = withStyle ~fg:Color.yellow e
let colorBlue e = withStyle ~fg:Color.blue e
let colorMagenta e = withStyle ~fg:Color.magenta e
let colorCyan e = withStyle ~fg:Color.cyan e
let colorWhite e = withStyle ~fg:Color.white e
let colorBrightBlack e = withStyle ~fg:Color.brightBlack e
let colorBrightRed e = withStyle ~fg:Color.brightRed e
let colorBrightGreen e = withStyle ~fg:Color.brightGreen e
let colorBrightYellow e = withStyle ~fg:Color.brightYellow e
let colorBrightBlue e = withStyle ~fg:Color.brightBlue e
let colorBrightMagenta e = withStyle ~fg:Color.brightMagenta e
let colorBrightCyan e = withStyle ~fg:Color.brightCyan e
let colorBrightWhite e = withStyle ~fg:Color.brightWhite e
let colorRGB r g b e = withStyle ~fg:(Color.rgb r g b) e
let color256 n e = withStyle ~fg:(Color.c256 n) e

(** Background color functions - pipe-friendly *)
let bgBlack e = withStyle ~bg:Color.black e

let bgRed e = withStyle ~bg:Color.red e
let bgGreen e = withStyle ~bg:Color.green e
let bgYellow e = withStyle ~bg:Color.yellow e
let bgBlue e = withStyle ~bg:Color.blue e
let bgMagenta e = withStyle ~bg:Color.magenta e
let bgCyan e = withStyle ~bg:Color.cyan e
let bgWhite e = withStyle ~bg:Color.white e
let bgBrightBlack e = withStyle ~bg:Color.brightBlack e
let bgBrightRed e = withStyle ~bg:Color.brightRed e
let bgBrightGreen e = withStyle ~bg:Color.brightGreen e
let bgBrightYellow e = withStyle ~bg:Color.brightYellow e
let bgBrightBlue e = withStyle ~bg:Color.brightBlue e
let bgBrightMagenta e = withStyle ~bg:Color.brightMagenta e
let bgBrightCyan e = withStyle ~bg:Color.brightCyan e
let bgBrightWhite e = withStyle ~bg:Color.brightWhite e
let bgRGB r g b e = withStyle ~bg:(Color.rgb r g b) e
let bg256 n e = withStyle ~bg:(Color.c256 n) e

(** Style functions - pipe-friendly *)
let styleBold e = withStyle ~style:Style.bold e

let styleDim e = withStyle ~style:Style.dim e
let styleItalic e = withStyle ~style:Style.italic e
let styleUnderline e = withStyle ~style:Style.underline e
let styleBlink e = withStyle ~style:Style.blink e
let styleReverse e = withStyle ~style:Style.reverse e
let styleHidden e = withStyle ~style:Style.hidden e
let styleStrikethrough e = withStyle ~style:Style.strikethrough e

(** Render element to string *)
let render = render_el

(** Get element width *)
let width = width_el

(** Get element height *)
let height = height_el

(** Print element to stdout *)
let print element = print_endline (render element)
