(*
 * +==========================================================================+
 * |                               layoutz                                    |
 * |               Friendly, expressive print-layout DSL                      |
 * |                                                                          |
 * | Copyright 2026 Matthieu Court (matthieu.court@protonmail.com)            |
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
    | Ascii
    | Block
    | Dashed
    | Dotted
    | InnerHalfBlock
    | OuterHalfBlock
    | Markdown
    | Custom of string * string * string  (** corner, horizontal, vertical *)
    | None

  type border_chars = {
    tl: string;
    tr: string;
    bl: string;
    br: string;
    h_top: string;
    h_bottom: string;
    v_left: string;
    v_right: string;
    left_tee: string;
    right_tee: string;
    cross: string;
    top_tee: string;
    bottom_tee: string;
  }

  let mk tl tr bl br h v lt rt cross tt bt =
    {
      tl;
      tr;
      bl;
      br;
      h_top = h;
      h_bottom = h;
      v_left = v;
      v_right = v;
      left_tee = lt;
      right_tee = rt;
      cross;
      top_tee = tt;
      bottom_tee = bt;
    }

  let all_chars = function
    | Normal -> mk "┌" "┐" "└" "┘" "─" "│" "├" "┤" "┼" "┬" "┴"
    | Double -> mk "╔" "╗" "╚" "╝" "═" "║" "╠" "╣" "╬" "╦" "╩"
    | Thick -> mk "┏" "┓" "┗" "┛" "━" "┃" "┣" "┫" "╋" "┳" "┻"
    | Round -> mk "╭" "╮" "╰" "╯" "─" "│" "├" "┤" "┼" "┬" "┴"
    | Ascii -> mk "+" "+" "+" "+" "-" "|" "+" "+" "+" "+" "+"
    | Block -> mk "█" "█" "█" "█" "█" "█" "█" "█" "█" "█" "█"
    | Dashed -> mk "┌" "┐" "└" "┘" "╌" "╎" "├" "┤" "┼" "┬" "┴"
    | Dotted -> mk "┌" "┐" "└" "┘" "┈" "┊" "├" "┤" "┼" "┬" "┴"
    | InnerHalfBlock ->
      {
        tl = "▗";
        tr = "▖";
        bl = "▝";
        br = "▘";
        h_top = "▄";
        h_bottom = "▀";
        v_left = "▐";
        v_right = "▌";
        left_tee = "▐";
        right_tee = "▌";
        cross = "▄";
        top_tee = "▄";
        bottom_tee = "▀";
      }
    | OuterHalfBlock ->
      {
        tl = "▛";
        tr = "▜";
        bl = "▙";
        br = "▟";
        h_top = "▀";
        h_bottom = "▄";
        v_left = "▌";
        v_right = "▐";
        left_tee = "▌";
        right_tee = "▐";
        cross = "▀";
        top_tee = "▀";
        bottom_tee = "▄";
      }
    | Markdown -> mk "|" "|" "|" "|" "-" "|" "|" "|" "|" "|" "|"
    | Custom (corner, h, v) ->
      mk corner corner corner corner h v corner corner corner corner corner
    | None -> mk " " " " " " " " " " " " " " " " " " " " " "
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

(** Check if a Unicode codepoint is double-width in terminals (emojis, CJK,
    fullwidth) *)
let is_wide_codepoint cp =
  (cp >= 0x1100 && cp <= 0x115F)
  (* Hangul Jamo *)
  || (cp >= 0x2E80 && cp <= 0x9FFF)
  (* CJK radicals through Unified Ideographs *)
  || (cp >= 0xAC00 && cp <= 0xD7AF)
  (* Hangul Syllables *)
  || (cp >= 0xF900 && cp <= 0xFAFF)
  (* CJK Compatibility Ideographs *)
  || (cp >= 0xFE30 && cp <= 0xFE4F)
  (* CJK Compatibility Forms *)
  || (cp >= 0xFF01 && cp <= 0xFF60)
  (* Fullwidth Forms *)
  || (cp >= 0xFFE0 && cp <= 0xFFE6)
  (* Fullwidth Signs *)
  || (cp >= 0x1F000 && cp <= 0x1FFFF)
  (* Emojis, Mahjong, Playing Cards *)
  || (cp >= 0x20000 && cp <= 0x2FA1F)
  ||
  (* CJK Ext B-F, Compat Supplement *)
  (cp >= 0x2600 && cp <= 0x27BF)
(* Misc Symbols, Dingbats *)

(** Calculate display width of a string, accounting for double-width characters
*)
let display_width s =
  let len = String.length s in
  let rec count i acc =
    if i >= len
    then acc
    else
      let c = Char.code s.[i] in
      if c < 128
      then count (i + 1) (acc + 1)
      else if c land 0xC0 = 0x80
      then count (i + 1) acc (* continuation byte *)
      else
        let cp, next =
          if c < 0xE0 && i + 1 < len
          then (((c land 0x1F) lsl 6) lor (Char.code s.[i + 1] land 0x3F), i + 2)
          else if c < 0xF0 && i + 2 < len
          then
            ( ((c land 0x0F) lsl 12)
              lor ((Char.code s.[i + 1] land 0x3F) lsl 6)
              lor (Char.code s.[i + 2] land 0x3F),
              i + 3 )
          else if i + 3 < len
          then
            ( ((c land 0x07) lsl 18)
              lor ((Char.code s.[i + 1] land 0x3F) lsl 12)
              lor ((Char.code s.[i + 2] land 0x3F) lsl 6)
              lor (Char.code s.[i + 3] land 0x3F),
              i + 4 )
          else (c, i + 1)
        in
        let w = if is_wide_codepoint cp then 2 else 1 in
        count next (acc + w)
  in
  count 0 0

(** Calculate visible length of a string (ignoring ANSI codes, accounting for
    double-width characters) *)
let visible_length s = display_width (strip_ansi s)

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
let borderAscii e = set_border Border.Ascii e
let borderBlock e = set_border Border.Block e
let borderDashed e = set_border Border.Dashed e
let borderDotted e = set_border Border.Dotted e
let borderInnerHalfBlock e = set_border Border.InnerHalfBlock e
let borderOuterHalfBlock e = set_border Border.OuterHalfBlock e
let borderMarkdown e = set_border Border.Markdown e
let borderCustom ~corner ~h ~v e = set_border (Border.Custom (corner, h, v)) e

let borderNone = function
  | B ((module M), v) -> B ((module M), M.with_border v Border.None)
  | Bordered (_, inner) -> inner
  | other -> other

(* ============================================================================
   ANSI Colors and Styles
   ============================================================================ *)

module Color = struct
  type t = None | Code of int | C256 of int | RGB of int * int * int

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
  type t = { fg: Color.t; bg: Color.t; style: Style.t; inner: element }

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
      let reset = "\027[0m" in
      (* Re-apply outer style after any inner reset so border chars stay colored *)
      let reapply line =
        let rlen = String.length reset in
        let buf = Buffer.create (String.length line + 64) in
        let rec scan i =
          if i >= String.length line
          then ()
          else if
            i + rlen <= String.length line && String.sub line i rlen = reset
          then begin
            Buffer.add_string buf reset;
            Buffer.add_string buf start_code;
            scan (i + rlen)
          end
          else begin
            Buffer.add_char buf line.[i];
            scan (i + 1)
          end
        in
        scan 0; Buffer.contents buf
      in
      let content = render_el t.inner in
      let ls = lines content in
      unlines (List.map (fun line -> start_code ^ reapply line ^ end_code) ls)

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
  type t = { char: string; rule_width: int }

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
  type t = { char: string; rule_height: int }

  let create ?(char = "│") ?(rule_height = 10) () = { char; rule_height }
  let render t = String.concat "\n" (List.init t.rule_height (fun _ -> t.char))
  let width t = visible_length t.char
  let height t = t.rule_height
end

(** Centered element *)
module Centered = struct
  type t = { inner: element; target_width: int }

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
  type t = { inner: element; target_width: int }

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
  type t = { inner: element; target_width: int }

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
  type t = { inner: element; max_width: int; ellipsis: string }

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
        (* Byte truncation... TODO check if this works for all UTF-8, ASCII should be fine *)
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
  type t = { inner: element; max_width: int }

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
  type t = { inner: element; target_width: int; justify_last: bool }

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
  type t = { inner: element; padding: int }

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
  type t = { prefix: string; color: Color.t option; inner: element }

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
  type t = { inner: element; char: string; color: Color.t option }

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
  type t = { elements: element list; spacing: int }

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
      let padded_arr = Array.of_list (List.map Array.of_list padded) in
      let transposed =
        List.init max_height (fun row ->
            String.concat sep
              (Array.to_list (Array.map (fun col -> col.(row)) padded_arr)))
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
  let bar_width = 20

  type t = { label: string; progress: float }

  let create ~label ~progress = { label; progress = max 0.0 (min 1.0 progress) }

  let render t =
    let filled = int_of_float (t.progress *. float_of_int bar_width) in
    let empty = bar_width - filled in
    let bar = repeat_str filled "█" ^ repeat_str empty "─" in
    let pct = int_of_float (t.progress *. 100.0) in
    Printf.sprintf "%s [%s] %d%%" t.label bar pct

  (* " [" = 2, "] " = 2, "100%" = 4 *)
  let width t = visible_length t.label + 2 + bar_width + 2 + 4
  let height _ = 1
end

(** Chart - horizontal bar chart *)
module Chart = struct
  let label_cap = 15
  let bar_width = 40
  let value_col_width = 10

  type t = (string * float) list

  let create data = data

  let render t =
    if t = []
    then "No data"
    else
      let max_value = List.fold_left (fun acc (_, v) -> max acc v) 0.0 t in
      let max_label_width =
        min label_cap
          (List.fold_left (fun acc (l, _) -> max acc (String.length l)) 0 t)
      in
      let render_bar (label, value) =
        let truncated_label =
          if String.length label > max_label_width
          then String.sub label 0 (max_label_width - 3) ^ "..."
          else label
        in
        let padded_label = pad_right max_label_width truncated_label in
        let pct = if max_value = 0.0 then 0.0 else value /. max_value in
        let bar_len = int_of_float (pct *. float_of_int bar_width) in
        let bar =
          repeat_str bar_len "█" ^ repeat_str (bar_width - bar_len) "─"
        in
        let value_str =
          if value = floor value
          then string_of_int (int_of_float value)
          else Printf.sprintf "%.1f" value
        in
        padded_label ^ " │" ^ bar ^ "│ " ^ value_str
      in
      unlines (List.map render_bar t)

  (* label " │" bar "│ " value *)
  let width _ = label_cap + 3 + bar_width + value_col_width
  let height t = max 1 (List.length t)
end

(* ============================================================================
   Visualization Helpers
   ============================================================================ *)

let default_palette =
  [|
    Color.red;
    Color.green;
    Color.blue;
    Color.yellow;
    Color.magenta;
    Color.cyan;
    Color.brightRed;
    Color.brightGreen;
    Color.brightBlue;
    Color.brightYellow;
    Color.brightMagenta;
    Color.brightCyan;
  |]

let pick_color idx default_c =
  match default_c with
  | Color.None ->
    let n = Array.length default_palette in
    default_palette.(idx mod n)
  | c -> c

let wrap_ansi_fg color text =
  match color with
  | Color.None -> text
  | c -> "\027[" ^ Color.fg_code c ^ "m" ^ text ^ "\027[0m"

let wrap_ansi_bg_256 c256 text =
  "\027[48;5;" ^ string_of_int c256 ^ "m" ^ text ^ "\027[0m"

(* Format a number for chart axis labels: integer when possible,
   1 decimal for normal range, scientific for extreme values *)
let format_axis_num v =
  let abs_v = Float.abs v in
  if v = Float.round v && abs_v < 1e9
  then string_of_int (int_of_float v)
  else if abs_v >= 0.01 && abs_v < 1e6
  then Printf.sprintf "%.1f" v
  else Printf.sprintf "%.1e" v

let braille_dot dy dx =
  match (dy, dx) with
  | 0, 0 -> 0x01
  | 1, 0 -> 0x02
  | 2, 0 -> 0x04
  | 3, 0 -> 0x40
  | 0, 1 -> 0x08
  | 1, 1 -> 0x10
  | 2, 1 -> 0x20
  | 3, 1 -> 0x80
  | _ -> 0

let braille_str bits =
  if bits = 0
  then " "
  else
    let cp = 0x2800 + bits in
    let buf = Bytes.create 3 in
    Bytes.set buf 0 (Char.chr (0xE0 lor (cp lsr 12)));
    Bytes.set buf 1 (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Bytes.set buf 2 (Char.chr (0x80 lor (cp land 0x3F)));
    Bytes.to_string buf

let block_chars = [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |]

(* ============================================================================
   Spinner
   ============================================================================ *)

module SpinnerStyle = struct
  type t = Dots | Line | Clock | Bounce | Earth | Moon | Grow | Arrow

  let frames = function
    | Dots -> [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
    | Line -> [| "|"; "/"; "-"; "\\" |]
    | Clock -> [| "🕐"; "🕑"; "🕒"; "🕓"; "🕔"; "🕕"; "🕖"; "🕗"; "🕘"; "🕙"; "🕚"; "🕛" |]
    | Bounce -> [| "⠁"; "⠂"; "⠄"; "⠂" |]
    | Earth -> [| "🌍"; "🌎"; "🌏" |]
    | Moon -> [| "🌑"; "🌒"; "🌓"; "🌔"; "🌕"; "🌖"; "🌗"; "🌘" |]
    | Grow -> [| "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
    | Arrow -> [| "←"; "↖"; "↑"; "↗"; "→"; "↘"; "↓"; "↙" |]
end

type spinner_style = SpinnerStyle.t

module Spinner = struct
  type t = { label: string; frame: int; style: SpinnerStyle.t }

  let create ~label ~frame ~style = { label; frame; style }

  let render t =
    let frames = SpinnerStyle.frames t.style in
    let spin_char = frames.(t.frame mod Array.length frames) in
    if t.label = "" then spin_char else spin_char ^ " " ^ t.label

  let width t =
    let frames = SpinnerStyle.frames t.style in
    let spin_w = visible_length frames.(0) in
    if t.label = "" then spin_w else spin_w + 1 + visible_length t.label

  let height _ = 1
end

(* ============================================================================
   Sparkline
   ============================================================================ *)

module Sparkline = struct
  type t = float list

  let create vals = vals

  let render t =
    match t with
    | [] -> ""
    | _ ->
      let mn = List.fold_left min infinity t in
      let mx = List.fold_left max neg_infinity t in
      let rng = mx -. mn in
      let idx v =
        if rng = 0.0
        then 4
        else
          max 1 (min 8 (int_of_float (Float.round ((v -. mn) /. rng *. 8.0))))
      in
      String.concat "" (List.map (fun v -> block_chars.(idx v)) t)

  let width t = List.length t
  let height _ = 1
end

(* ============================================================================
   Braille Line/Scatter Plot
   ============================================================================ *)

type series = {
  points: (float * float) list;
  label: string;
  series_color: Color.t;
}

module PlotLine = struct
  type t = { series: series list; plot_width: int; plot_height: int }

  let create ~width ~height series =
    { series; plot_width = width; plot_height = height }

  let render t =
    let all_pts = List.concat_map (fun s -> s.points) t.series in
    match all_pts with
    | [] -> "No data"
    | _ ->
      let xs = List.map fst all_pts in
      let ys = List.map snd all_pts in
      let x_min = List.fold_left min infinity xs in
      let x_max = List.fold_left max neg_infinity xs in
      let y_min = List.fold_left min infinity ys in
      let y_max = List.fold_left max neg_infinity ys in
      let x_rng = if x_max = x_min then 1.0 else x_max -. x_min in
      let y_rng = if y_max = y_min then 1.0 else y_max -. y_min in
      let px_w = t.plot_width * 2 in
      let px_h = t.plot_height * 4 in
      let clamp lo hi v = max lo (min hi v) in
      let to_pixel (x, y) =
        let px =
          clamp 0 (px_w - 1)
            (int_of_float
               (Float.round ((x -. x_min) /. x_rng *. float_of_int (px_w - 1))))
        in
        let py =
          clamp 0 (px_h - 1)
            (int_of_float
               (Float.round ((y_max -. y) /. y_rng *. float_of_int (px_h - 1))))
        in
        (px, py)
      in
      (* Grid: array of (bits, series_index) *)
      let grid =
        Array.init t.plot_height (fun _ ->
            Array.init t.plot_width (fun _ -> (0, -1)))
      in
      let series_arr = Array.of_list t.series in
      List.iteri
        (fun si s ->
          List.iter
            (fun pt ->
              let px, py = to_pixel pt in
              let cx = px / 2 and cy = py / 4 in
              let dx = px mod 2 and dy = py mod 4 in
              let bit = braille_dot dy dx in
              if cy >= 0 && cy < t.plot_height && cx >= 0 && cx < t.plot_width
              then begin
                let old_bits, old_si = grid.(cy).(cx) in
                grid.(cy).(cx) <-
                  (old_bits lor bit, if old_si < 0 then si else old_si)
              end)
            s.points)
        t.series;
      let y_ticks =
        List.init t.plot_height (fun i ->
            y_max
            -. y_rng
               *. float_of_int i
               /. float_of_int (max 1 (t.plot_height - 1)))
      in
      let y_labels = List.map format_axis_num y_ticks in
      let y_label_w =
        List.fold_left (fun acc l -> max acc (String.length l)) 0 y_labels
      in
      let grid_lines =
        List.mapi
          (fun row_idx y_lbl ->
            let label_padded = pad_left y_label_w y_lbl in
            let cells =
              Array.to_list
                (Array.mapi
                   (fun col _ ->
                     let bits, si = grid.(row_idx).(col) in
                     let ch = braille_str bits in
                     if si >= 0
                     then
                       let c = pick_color si series_arr.(si).series_color in
                       wrap_ansi_fg c ch
                     else ch)
                   grid.(row_idx))
            in
            label_padded ^ " │" ^ String.concat "" cells)
          y_labels
      in
      (* X axis *)
      let x_axis =
        String.make y_label_w ' ' ^ " └" ^ repeat_str t.plot_width "─"
      in
      let x_min_l = format_axis_num x_min in
      let x_max_l = format_axis_num x_max in
      let x_labels =
        String.make (y_label_w + 2) ' '
        ^ x_min_l
        ^ String.make
            (max 1
               (t.plot_width - String.length x_min_l - String.length x_max_l))
            ' '
        ^ x_max_l
      in
      (* Legend *)
      let legend =
        if List.length t.series <= 1
        then []
        else
          let items =
            List.mapi
              (fun i s ->
                let c = pick_color i s.series_color in
                wrap_ansi_fg c "●" ^ " " ^ s.label)
              t.series
          in
          [ ""; String.concat "  " items ]
      in
      unlines (grid_lines @ [ x_axis; x_labels ] @ legend)

  let width t = 8 + t.plot_width
  let height t = t.plot_height + 2
end

(* ============================================================================
   Braille Pie Chart
   ============================================================================ *)

type slice = { value: float; slice_label: string; slice_color: Color.t }

module PlotPie = struct
  type t = { slices: slice list; pie_width: int; pie_height: int }

  let create ~width ~height slices =
    { slices; pie_width = width; pie_height = height }

  let render t =
    match t.slices with
    | [] -> "No data"
    | _ ->
      let pi = Float.pi in
      let total = List.fold_left (fun acc s -> acc +. s.value) 0.0 t.slices in
      if total = 0.0
      then "No data"
      else
        let angles =
          List.map (fun s -> s.value /. total *. 2.0 *. pi) t.slices
        in
        let cum_angles =
          let rec scan acc = function
            | [] -> List.rev acc
            | a :: rest ->
              let next = List.hd acc +. a in
              scan (next :: acc) rest
          in
          scan [ 0.0 ] angles
        in
        let find_slice ang =
          let rec go i = function
            | [] -> max 0 (i - 1)
            | a :: rest -> if ang < a then i else go (i + 1) rest
          in
          go 0 (List.tl cum_angles)
        in
        let slices_arr = Array.of_list t.slices in
        let n_slices = Array.length slices_arr in
        let cx_f = float_of_int t.pie_width in
        let cy_f = float_of_int (t.pie_height * 4) /. 2.0 in
        let radius = min cx_f (cy_f *. 0.9) in
        let grid_lines =
          List.init t.pie_height (fun gcy ->
              String.concat ""
                (List.init t.pie_width (fun gcx ->
                     let sub_px =
                       List.init 8 (fun idx ->
                           let dy = idx / 2 and dx = idx mod 2 in
                           let dpx = float_of_int ((gcx * 2) + dx) in
                           let dpy = float_of_int ((gcy * 4) + dy) in
                           let rel_x = dpx -. cx_f in
                           let rel_y = (dpy -. cy_f) *. 2.0 in
                           let dist =
                             sqrt ((rel_x *. rel_x) +. (rel_y *. rel_y))
                           in
                           let ang = atan2 rel_y rel_x in
                           let n_ang =
                             if ang < 0.0 then ang +. (2.0 *. pi) else ang
                           in
                           (dy, dx, dist, n_ang))
                     in
                     let inside =
                       List.filter_map
                         (fun (dy, dx, dist, n_ang) ->
                           if dist <= radius then Some (dy, dx, n_ang) else None)
                         sub_px
                     in
                     let bits =
                       List.fold_left
                         (fun acc (dy, dx, _) -> acc lor braille_dot dy dx)
                         0 inside
                     in
                     let dom_si =
                       match inside with
                       | [] -> -1
                       | (_, _, a) :: _ -> find_slice a
                     in
                     if bits = 0
                     then " "
                     else
                       let ch = braille_str bits in
                       if dom_si >= 0 && dom_si < n_slices
                       then
                         let sl = slices_arr.(dom_si) in
                         let c = pick_color dom_si sl.slice_color in
                         wrap_ansi_fg c ch
                       else ch)))
        in
        let legend_lines =
          List.mapi
            (fun i sl ->
              let c = pick_color i sl.slice_color in
              let pct = Printf.sprintf "%.0f" (sl.value /. total *. 100.0) in
              "  "
              ^ wrap_ansi_fg c "●"
              ^ " "
              ^ sl.slice_label
              ^ " ("
              ^ pct
              ^ "%)")
            t.slices
        in
        unlines (grid_lines @ [ "" ] @ legend_lines)

  let width t = t.pie_width
  let height t = t.pie_height + 1 + List.length t.slices
end

(* ============================================================================
   Vertical Bar Chart
   ============================================================================ *)

type bar_item = { bar_value: float; bar_label: string; bar_color: Color.t }

module PlotBar = struct
  type t = { items: bar_item list; chart_width: int; chart_height: int }

  let create ~width ~height items =
    { items; chart_width = width; chart_height = height }

  let render t =
    match t.items with
    | [] -> "No data"
    | _ ->
      let max_val =
        List.fold_left (fun acc b -> max acc b.bar_value) 0.0 t.items
      in
      if max_val = 0.0
      then "No data"
      else
        let n_bars = List.length t.items in
        let bar_w = max 1 ((t.chart_width - n_bars + 1) / n_bars) in
        let total_sub = t.chart_height * 8 in
        let bar_hts =
          List.map
            (fun b ->
              int_of_float
                (Float.round (b.bar_value /. max_val *. float_of_int total_sub)))
            t.items
        in
        let y_ticks =
          List.init t.chart_height (fun i ->
              max_val
              *. float_of_int (t.chart_height - 1 - i)
              /. float_of_int (max 1 (t.chart_height - 1)))
        in
        let y_labels = List.map format_axis_num y_ticks in
        let y_label_w =
          List.fold_left (fun acc l -> max acc (String.length l)) 0 y_labels
        in
        let bars_with_hts = List.combine t.items bar_hts in
        let grid_lines =
          List.mapi
            (fun row_idx y_lbl ->
              let r = t.chart_height - 1 - row_idx in
              let bar_cells =
                String.concat " "
                  (List.mapi
                     (fun i (b, bh) ->
                       let filled = min 8 (max 0 (bh - (r * 8))) in
                       let color = pick_color i b.bar_color in
                       let bar_str = repeat_str bar_w block_chars.(filled) in
                       if filled > 0
                       then wrap_ansi_fg color bar_str
                       else bar_str)
                     bars_with_hts)
              in
              pad_left y_label_w y_lbl ^ " │" ^ bar_cells)
            y_labels
        in
        let x_axis_w = (n_bars * bar_w) + n_bars - 1 in
        let x_axis =
          String.make y_label_w ' ' ^ " └" ^ repeat_str x_axis_w "─"
        in
        let bar_labels =
          String.make (y_label_w + 2) ' '
          ^ String.concat " "
              (List.map
                 (fun b ->
                   let lbl = b.bar_label in
                   if String.length lbl >= bar_w
                   then String.sub lbl 0 bar_w
                   else lbl ^ String.make (bar_w - String.length lbl) ' ')
                 t.items)
        in
        unlines (grid_lines @ [ x_axis; bar_labels ])

  let width t = 8 + t.chart_width
  let height t = t.chart_height + 2
end

(* ============================================================================
   Stacked Bar Chart
   ============================================================================ *)

type stacked_bar_group = { segments: bar_item list; group_label: string }

module PlotStackedBar = struct
  type t = {
    groups: stacked_bar_group list;
    chart_width: int;
    chart_height: int;
  }

  let create ~width ~height groups =
    { groups; chart_width = width; chart_height = height }

  let render t =
    match t.groups with
    | [] -> "No data"
    | _ ->
      let max_total =
        List.fold_left
          (fun acc g ->
            max acc
              (List.fold_left (fun a s -> a +. s.bar_value) 0.0 g.segments))
          0.0 t.groups
      in
      if max_total = 0.0
      then "No data"
      else
        let n_groups = List.length t.groups in
        let bar_w = max 1 ((t.chart_width - n_groups + 1) / n_groups) in
        let total_sub = t.chart_height * 8 in
        (* For each group, compute cumulative sub-pixel heights *)
        let group_bounds =
          List.map
            (fun g ->
              let sub_hts =
                List.map
                  (fun s ->
                    int_of_float
                      (Float.round
                         (s.bar_value /. max_total *. float_of_int total_sub)))
                  g.segments
              in
              let rec cumulate acc = function
                | [] -> List.rev acc
                | h :: rest ->
                  let next = List.hd acc + h in
                  cumulate (next :: acc) rest
              in
              let cum = cumulate [ 0 ] sub_hts in
              let bottoms =
                List.filteri (fun i _ -> i < List.length g.segments) cum
              in
              let tops = List.tl cum in
              List.map2
                (fun seg (bot, top) -> (seg, bot, top))
                g.segments
                (List.combine bottoms tops))
            t.groups
        in
        (* Collect all unique segment labels *)
        let all_labels =
          let seen = Hashtbl.create 8 in
          List.concat_map
            (fun g ->
              List.filter_map
                (fun s ->
                  if Hashtbl.mem seen s.bar_label
                  then None
                  else begin
                    Hashtbl.add seen s.bar_label ();
                    Some s.bar_label
                  end)
                g.segments)
            t.groups
        in
        let label_idx_tbl = Hashtbl.create (List.length all_labels) in
        List.iteri (fun i nm -> Hashtbl.replace label_idx_tbl nm i) all_labels;
        let label_idx nm =
          try Hashtbl.find label_idx_tbl nm with Not_found -> 0
        in
        let y_ticks =
          List.init t.chart_height (fun i ->
              max_total
              *. float_of_int (t.chart_height - 1 - i)
              /. float_of_int (max 1 (t.chart_height - 1)))
        in
        let y_labels = List.map format_axis_num y_ticks in
        let y_label_w =
          List.fold_left (fun acc l -> max acc (String.length l)) 0 y_labels
        in
        let grid_lines =
          List.mapi
            (fun row_idx y_lbl ->
              let r = t.chart_height - 1 - row_idx in
              let sub_bot = r * 8 in
              let sub_top = (r * 8) + 8 in
              let bar_cells =
                String.concat " "
                  (List.map
                     (fun bounds ->
                       let overlapping =
                         List.filter
                           (fun (_, bot, top) -> top > sub_bot && bot < sub_top)
                           bounds
                       in
                       match overlapping with
                       | [] -> String.make bar_w ' '
                       | _ ->
                         let top_seg, _, top_val =
                           List.fold_left
                             (fun ((_, _, best_top) as best) ((_, _, t) as cur)
                                -> if t > best_top then cur else best)
                             (List.hd overlapping) overlapping
                         in
                         let filled = min 8 (max 0 (top_val - sub_bot)) in
                         let color =
                           match top_seg.bar_color with
                           | Color.None ->
                             pick_color (label_idx top_seg.bar_label) Color.None
                           | c -> c
                         in
                         let bar_str = repeat_str bar_w block_chars.(filled) in
                         if filled > 0
                         then wrap_ansi_fg color bar_str
                         else bar_str)
                     group_bounds)
              in
              pad_left y_label_w y_lbl ^ " │" ^ bar_cells)
            y_labels
        in
        let x_axis_w = (n_groups * bar_w) + n_groups - 1 in
        let x_axis =
          String.make y_label_w ' ' ^ " └" ^ repeat_str x_axis_w "─"
        in
        let grp_labels =
          String.make (y_label_w + 2) ' '
          ^ String.concat " "
              (List.map
                 (fun g ->
                   let lbl = g.group_label in
                   if String.length lbl >= bar_w
                   then String.sub lbl 0 bar_w
                   else lbl ^ String.make (bar_w - String.length lbl) ' ')
                 t.groups)
        in
        let legend =
          if List.length all_labels <= 1
          then []
          else
            let items =
              List.mapi
                (fun i nm ->
                  let c = pick_color i Color.None in
                  wrap_ansi_fg c "█" ^ " " ^ nm)
                all_labels
            in
            [ ""; String.concat "  " items ]
        in
        unlines (grid_lines @ [ x_axis; grp_labels ] @ legend)

  let width t = 8 + t.chart_width
  let height t = t.chart_height + 2
end

(* ============================================================================
   Heatmap
   ============================================================================ *)

type heatmap_data = {
  grid: float list list;
  row_labels: string list;
  col_labels: string list;
}

module Heatmap = struct
  type t = { data: heatmap_data; cell_width: int }

  let create ?(cell_width = 6) data = { data; cell_width }

  let render t =
    match t.data.grid with
    | [] -> "No data"
    | _ -> (
      let all_vals = List.concat t.data.grid in
      match all_vals with
      | [] -> "No data"
      | _ ->
        let mn = List.fold_left min infinity all_vals in
        let mx = List.fold_left max neg_infinity all_vals in
        let rng = if mx = mn then 1.0 else mx -. mn in
        let normalize v = (v -. mn) /. rng in
        (* xterm-256 blue(21) -> cyan(51) -> green(46) -> yellow(226) -> red(196) *)
        let to_color256 t =
          let lerp a b s = int_of_float (Float.round (a +. (s *. (b -. a)))) in
          if t <= 0.0
          then 21
          else if t >= 1.0
          then 196
          else if t < 0.25
          then lerp 21.0 51.0 (t /. 0.25)
          else if t < 0.5
          then lerp 51.0 46.0 ((t -. 0.25) /. 0.25)
          else if t < 0.75
          then lerp 46.0 226.0 ((t -. 0.5) /. 0.25)
          else lerp 226.0 196.0 ((t -. 0.75) /. 0.25)
        in
        let row_lbl_w =
          List.fold_left
            (fun acc l -> max acc (String.length l))
            0 t.data.row_labels
        in
        let row_labels_arr = Array.of_list t.data.row_labels in
        let header =
          String.make (row_lbl_w + 1) ' '
          ^ String.concat " "
              (List.map
                 (fun l ->
                   let truncated =
                     if String.length l > t.cell_width
                     then String.sub l 0 t.cell_width
                     else l
                   in
                   pad_right t.cell_width truncated)
                 t.data.col_labels)
        in
        let data_rows =
          List.mapi
            (fun i row_vals ->
              let lbl =
                if i < Array.length row_labels_arr
                then row_labels_arr.(i)
                else ""
              in
              let truncated_lbl =
                if String.length lbl > row_lbl_w
                then String.sub lbl 0 row_lbl_w
                else lbl
              in
              pad_right row_lbl_w truncated_lbl
              ^ " "
              ^ String.concat " "
                  (List.map
                     (fun v ->
                       let n = normalize v in
                       let c256 = to_color256 n in
                       let vs = format_axis_num v in
                       let truncated_vs =
                         if String.length vs > t.cell_width
                         then String.sub vs 0 t.cell_width
                         else vs
                       in
                       wrap_ansi_bg_256 c256
                         (pad_right t.cell_width truncated_vs))
                     row_vals))
            t.data.grid
        in
        let legend_cs =
          List.init 11 (fun i -> to_color256 (float_of_int i /. 10.0))
        in
        let legend_bar =
          String.concat ""
            (List.map (fun c -> wrap_ansi_bg_256 c " ") legend_cs)
        in
        let legend_line =
          String.make (row_lbl_w + 1) ' '
          ^ format_axis_num mn
          ^ " "
          ^ legend_bar
          ^ " "
          ^ format_axis_num mx
        in
        unlines ([ header ] @ data_rows @ [ ""; legend_line ]))

  let width t =
    let n_cols =
      match t.data.grid with
      | [] -> 0
      | r :: _ -> List.length r
    in
    let row_lbl_w =
      List.fold_left
        (fun acc l -> max acc (String.length l))
        0 t.data.row_labels
    in
    row_lbl_w + 1 + (n_cols * (t.cell_width + 1))

  let height t = List.length t.data.grid + 3
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
  type t = { elements: element list; tight: bool }

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
      let padded_arr = Array.of_list (List.map Array.of_list padded) in
      let transposed =
        List.init max_height (fun row ->
            String.concat separator
              (Array.to_list (Array.map (fun col -> col.(row)) padded_arr)))
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
  type t = { content: element; border: Border.t }

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
    let c = Border.all_chars t.border in
    let top = c.tl ^ repeat_str inner_width c.h_top ^ c.tr in
    let bot = c.bl ^ repeat_str inner_width c.h_bottom ^ c.br in
    let empty_line = c.v_left ^ String.make inner_width ' ' ^ c.v_right in
    let content_rendered =
      List.map
        (fun l -> c.v_left ^ "  " ^ pad_right max_width l ^ "  " ^ c.v_right)
        content_lines
    in
    unlines ([ top; empty_line ] @ content_rendered @ [ empty_line; bot ])

  let width t = width_el t.content + 8
  let height t = height_el t.content + 4
end

(** Box - bordered container (implements BORDERABLE) *)
module Box = struct
  type t = { title: string; elements: element list; border: Border.t }

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
    let c = Border.all_chars t.border in
    let top_border =
      if t.title = ""
      then c.tl ^ repeat_str (total_width - 2) c.h_top ^ c.tr
      else
        let title_padding = total_width - visible_length t.title - 2 in
        let left_pad = title_padding / 2 in
        let right_pad = title_padding - left_pad in
        c.tl
        ^ repeat_str left_pad c.h_top
        ^ t.title
        ^ repeat_str right_pad c.h_top
        ^ c.tr
    in
    let bottom_border = c.bl ^ repeat_str (total_width - 2) c.h_bottom ^ c.br in
    let padded_content =
      List.map
        (fun line ->
          c.v_left ^ " " ^ pad_right inner_width line ^ " " ^ c.v_right)
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
  type t = { headers: element list; rows: element list list; border: Border.t }

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
      let row_widths_arr =
        Array.of_list
          (List.map
             (fun row ->
               Array.of_list
                 (List.map
                    (fun cell ->
                      List.fold_left
                        (fun acc l -> max acc (visible_length l))
                        0 (lines cell))
                    row))
             rendered_rows)
      in
      List.mapi
        (fun i hw ->
          Array.fold_left (fun acc row -> max acc row.(i)) hw row_widths_arr)
        header_widths
    in
    let c = Border.all_chars t.border in
    let make_border left h conn right =
      let parts = List.map (fun w -> repeat_str w h) col_widths in
      left ^ h ^ String.concat (h ^ conn ^ h) parts ^ h ^ right
    in
    let top_border = make_border c.tl c.h_top c.top_tee c.tr in
    let sep_border = make_border c.left_tee c.h_top c.cross c.right_tee in
    let bot_border = make_border c.bl c.h_bottom c.bottom_tee c.br in
    let make_row cells =
      let padded = List.map2 pad_right col_widths cells in
      c.v_left
      ^ " "
      ^ String.concat (" " ^ c.v_left ^ " ") padded
      ^ " "
      ^ c.v_right
    in
    let header_row = make_row header_strs in
    let data_rows =
      List.map
        (fun row ->
          let cell_lines = List.map lines row in
          let max_h =
            List.fold_left (fun acc ls -> max acc (List.length ls)) 1 cell_lines
          in
          let padded_cells_arr =
            Array.of_list
              (List.map Array.of_list
                 (List.map2
                    (fun w cls ->
                      let padded = List.map (pad_right w) cls in
                      padded
                      @ List.init
                          (max_h - List.length cls)
                          (fun _ -> String.make w ' '))
                    col_widths cell_lines))
          in
          let transposed =
            List.init max_h (fun r ->
                Array.to_list (Array.map (fun col -> col.(r)) padded_cells_arr))
          in
          List.map
            (fun row_cells ->
              c.v_left
              ^ " "
              ^ String.concat (" " ^ c.v_left ^ " ") row_cells
              ^ " "
              ^ c.v_right)
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
      let row_widths_arr =
        Array.of_list
          (List.map
             (fun row ->
               Array.of_list
                 (List.map
                    (fun cell ->
                      List.fold_left
                        (fun acc l -> max acc (visible_length l))
                        0 (lines cell))
                    row))
             rendered_rows)
      in
      List.mapi
        (fun i hw ->
          Array.fold_left (fun acc row -> max acc row.(i)) hw row_widths_arr)
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
      let c = Border.all_chars border in
      let top = c.tl ^ repeat_str (max_width + 2) c.h_top ^ c.tr in
      let bot = c.bl ^ repeat_str (max_width + 2) c.h_bottom ^ c.br in
      let body =
        List.map
          (fun line ->
            c.v_left ^ " " ^ pad_right max_width line ^ " " ^ c.v_right)
          content_lines
      in
      unlines ([ top ] @ body @ [ bot ])

type list_item = { content: element; children: list_item list }
(** List item type - recursive for deep nesting *)

type tree_node = { label: element; branches: tree_node list }
(** Tree node type - recursive *)

(** Unordered list - items are (element, children) pairs *)
module UList = struct
  type t = { items: list_item list; bullet: string }

  let create ?(bullet = "•") items = { items; bullet }

  let rec render_with_indent indent level t =
    let prefix_indent = String.make indent ' ' in
    let bullet = t.bullet in
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
  type t = { items: list_item list; start: int }

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

(** Spinner - animated loading indicator *)
let spinner ~label ~frame ~style =
  el (module Spinner) (Spinner.create ~label ~frame ~style)

(** Sparkline - inline spark chart from values *)
let sparkline vals = el (module Sparkline) (Sparkline.create vals)

(** Series constructor for line/scatter plots *)
let series ~points ~label ~color = { points; label; series_color = color }

(** Braille line/scatter plot *)
let plotLine ~width ~height series_list =
  el (module PlotLine) (PlotLine.create ~width ~height series_list)

(** Slice constructor for pie charts *)
let slice ~value ~label ~color =
  { value; slice_label = label; slice_color = color }

(** Braille pie chart *)
let plotPie ~width ~height slices =
  el (module PlotPie) (PlotPie.create ~width ~height slices)

(** Bar item constructor for bar charts *)
let bar_item ~value ~label ~color =
  { bar_value = value; bar_label = label; bar_color = color }

(** Vertical bar chart *)
let plotBar ~width ~height items =
  el (module PlotBar) (PlotBar.create ~width ~height items)

(** Stacked bar group constructor *)
let stacked_group ~segments ~label = { segments; group_label = label }

(** Stacked vertical bar chart *)
let plotStackedBar ~width ~height groups =
  el (module PlotStackedBar) (PlotStackedBar.create ~width ~height groups)

(** Heatmap data constructor *)
let heatmap_data ~grid ~row_labels ~col_labels =
  { grid; row_labels; col_labels }

(** Heatmap visualization *)
let plotHeatmap ?(cell_width = 6) data =
  el (module Heatmap) (Heatmap.create ~cell_width data)

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

(* ============================================================================
   TUI Runtime - Elm Architecture for interactive terminal applications
   ============================================================================ *)

(** Keyboard input representation *)
type key =
  | KeyChar of char  (** Regular character keys: 'a', '1', ' ', etc. *)
  | KeyCtrl of char  (** Ctrl+key: KeyCtrl 'C', KeyCtrl 'Q', etc. *)
  | KeyEnter  (** Enter/Return key *)
  | KeyBackspace  (** Backspace key *)
  | KeyTab  (** Tab key *)
  | KeyEscape  (** Escape key *)
  | KeyDelete  (** Delete key *)
  | KeyUp  (** Up arrow *)
  | KeyDown  (** Down arrow *)
  | KeyLeft  (** Left arrow *)
  | KeyRight  (** Right arrow *)
  | KeyHome  (** Home key *)
  | KeyEnd  (** End key *)
  | KeyPageUp  (** Page Up *)
  | KeyPageDown  (** Page Down *)

(** Commands - side effects the runtime will execute after each update *)
type 'msg cmd =
  | CmdNone  (** No effect *)
  | CmdBatch of 'msg cmd list  (** Combine multiple commands *)
  | CmdTask of (unit -> 'msg option)
      (** Run a task, optionally produce a message *)
  | CmdAfterMs of int * 'msg  (** Fire a message after a delay *)
  | CmdExit  (** Exit the application *)

(** Subscriptions - event sources your app listens to *)
type 'msg sub =
  | SubNone  (** No subscriptions *)
  | SubKeyPress of (key -> 'msg option)  (** Subscribe to keyboard input *)
  | SubEveryMs of int * 'msg
      (** Subscribe to periodic ticks (ms interval + message) *)
  | SubBatch of 'msg sub list  (** Combine multiple subscriptions *)

(** App-level alignment within the terminal window *)
type alignment = AlignLeft | AlignCenter | AlignRight

type app_options = {
  alignment: alignment;
  quit_key: key;
  clear_on_start: bool;
  clear_on_exit: bool;
  render_interval_ms: int;
}
(** Options for running a LayoutzApp *)

(** Sensible defaults: left-aligned, Ctrl+Q to quit *)
let default_options =
  {
    alignment = AlignLeft;
    quit_key = KeyCtrl 'Q';
    clear_on_start = true;
    clear_on_exit = true;
    render_interval_ms = 33;
  }

type ('state, 'msg) app = {
  init: 'state * 'msg cmd;
  update: 'msg -> 'state -> 'state * 'msg cmd;
  subscriptions: 'state -> 'msg sub;
  view: 'state -> element;
}
(** The core application structure - Elm Architecture style.

    Build interactive TUI apps by defining:
    - Initial state and startup commands
    - How to update state based on messages
    - What events to subscribe to
    - How to render state to UI

    Example:
    {[
      type msg = Inc | Dec

      let counter_app =
        {
          init = (0, CmdNone);
          update =
            (fun msg count ->
              match msg with
              | Inc -> (count + 1, CmdNone)
              | Dec -> (count - 1, CmdNone));
          subscriptions =
            (fun _ ->
              sub_key_press (fun key ->
                  match key with
                  | KeyChar '+' -> Some Inc
                  | KeyChar '-' -> Some Dec
                  | _ -> None));
          view = (fun count -> layout [ s ("Count: " ^ string_of_int count) ]);
        }

      let () = run_app counter_app
    ]} *)

(* -- Convenience constructors -- *)

let cmd_none = CmdNone
let cmd_batch cmds = CmdBatch cmds
let cmd_task f = CmdTask f
let cmd_after_ms ms msg = CmdAfterMs (ms, msg)
let cmd_exit = CmdExit
let sub_none = SubNone
let sub_key_press handler = SubKeyPress handler
let sub_every_ms ms msg = SubEveryMs (ms, msg)
let sub_batch subs = SubBatch subs

(* -- Terminal control -- *)

let _enter_alt_screen () = Printf.printf "\027[?1049h%!"
let _exit_alt_screen () = Printf.printf "\027[?1049l%!"
let _clear_screen () = Printf.printf "\027[2J\027[H%!"
let _hide_cursor () = Printf.printf "\027[?25l%!"
let _show_cursor () = Printf.printf "\027[?25h%!"

let _terminal_width () =
  try
    let ic = Unix.open_process_in "stty size < /dev/tty 2>/dev/null" in
    let line = try input_line ic with End_of_file -> "24 80" in
    let _ = Unix.close_process_in ic in
    match String.split_on_char ' ' (String.trim line) with
    | [ _; cols ] -> ( try int_of_string cols with _ -> 80)
    | _ -> 80
  with _ -> 80

(* -- Raw terminal mode via tcsetattr (zero dependencies) -- *)

let _enter_raw_mode () =
  let fd = Unix.stdin in
  let old_attr = Unix.tcgetattr fd in
  let new_attr =
    {
      old_attr with
      Unix.c_icanon = false;
      Unix.c_echo = false;
      Unix.c_isig = false;
      Unix.c_ixon = false;
      Unix.c_vmin = 1;
      Unix.c_vtime = 0;
    }
  in
  Unix.tcsetattr fd Unix.TCSANOW new_attr;
  old_attr

let _exit_raw_mode attr = Unix.tcsetattr Unix.stdin Unix.TCSANOW attr

(* -- Key reading and escape sequence parsing -- *)

(* Time to wait for subsequent bytes of an escape sequence before
   treating ESC as a standalone keypress. *)
let _esc_timeout_ms = 50

let _read_byte_timeout_ms ms =
  let timeout = float_of_int ms /. 1000.0 in
  match Unix.select [ Unix.stdin ] [] [] timeout with
  | [], _, _ -> None
  | _ ->
    let buf = Bytes.create 1 in
    let n = Unix.read Unix.stdin buf 0 1 in
    if n = 0 then None else Some (Bytes.get buf 0)

let _read_byte () =
  let buf = Bytes.create 1 in
  let _ = Unix.read Unix.stdin buf 0 1 in
  Bytes.get buf 0

let rec _read_escape_sequence () =
  match _read_byte_timeout_ms _esc_timeout_ms with
  | None -> KeyEscape
  | Some '[' -> _parse_csi ()
  | Some _ -> KeyEscape

and _parse_csi () =
  match _read_byte_timeout_ms _esc_timeout_ms with
  | Some 'A' -> KeyUp
  | Some 'B' -> KeyDown
  | Some 'C' -> KeyRight
  | Some 'D' -> KeyLeft
  | Some 'H' -> KeyHome
  | Some 'F' -> KeyEnd
  | Some '3' -> _consume_tilde KeyDelete
  | Some '5' -> _consume_tilde KeyPageUp
  | Some '6' -> _consume_tilde KeyPageDown
  | _ -> KeyEscape

and _consume_tilde key =
  match _read_byte_timeout_ms _esc_timeout_ms with
  | Some '~' -> key
  | _ -> KeyEscape

let _read_key () =
  let c = _read_byte () in
  match Char.code c with
  | 10 | 13 -> KeyEnter
  | 27 -> _read_escape_sequence ()
  | 9 -> KeyTab
  | 127 | 8 -> KeyBackspace
  | n when n >= 32 && n <= 126 -> KeyChar c
  | n when n >= 1 && n < 32 -> KeyCtrl (Char.chr (n + 64))
  | _ -> KeyChar c

(* -- Subscription helpers -- *)

let rec _get_key_handlers = function
  | SubKeyPress handler -> [ handler ]
  | SubBatch subs -> List.concat_map _get_key_handlers subs
  | _ -> []

let rec _get_tick_subs = function
  | SubEveryMs (ms, msg) -> [ (ms, msg) ]
  | SubBatch subs -> List.concat_map _get_tick_subs subs
  | _ -> []

(* -- Text input helper -- *)

(** Handle a key for a text field.

    Returns [Some new_value] if the key was handled, [None] if not. Only handles
    keys when [field_id = active_field].

    Example:
    {[
      match
        input_handle key ~field_id:0 ~active_field:state.active
          ~current_value:state.name
      with
      | Some v -> Some (UpdateName v)
      | None -> None
    ]} *)
let input_handle key ~field_id ~active_field ~current_value =
  if field_id <> active_field
  then None
  else
    match key with
    | KeyChar c when c >= ' ' && c <= '~' ->
      Some (current_value ^ String.make 1 c)
    | KeyBackspace when String.length current_value > 0 ->
      Some (String.sub current_value 0 (String.length current_value - 1))
    | _ -> None

(* -- The runtime -- *)

(** Run an interactive TUI application.

    Sets up raw terminal mode, enters the event loop, and restores terminal on
    exit. Uses differential rendering - only redraws when the view output
    changes.

    Press the quit key (default: Ctrl+Q) to exit. *)
let _run_app_internal ?(options = default_options) (app : ('state, 'msg) app) =
  let old_attr = _enter_raw_mode () in
  if options.clear_on_start
  then begin
    _enter_alt_screen (); _clear_screen ()
  end;
  _hide_cursor ();

  let term_width = _terminal_width () in
  let initial_state, initial_cmd = app.init in

  let state = ref initial_state in
  let state_mutex = Mutex.create () in
  let should_continue = ref true in
  let last_rendered = ref "" in
  let last_line_count = ref 0 in

  let with_state f =
    Mutex.lock state_mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock state_mutex) f
  in

  let rec process_cmd cmd =
    match cmd with
    | CmdNone -> ()
    | CmdExit -> should_continue := false
    | CmdBatch cmds -> List.iter process_cmd cmds
    | CmdTask f ->
      ignore
        (Thread.create
           (fun () ->
             match try f () with _ -> None with
             | Some msg -> update_state msg
             | None -> ())
           ())
    | CmdAfterMs (ms, msg) ->
      ignore
        (Thread.create
           (fun () ->
             Thread.delay (float_of_int ms /. 1000.0);
             update_state msg)
           ())
  and update_state msg =
    with_state (fun () ->
        let new_state, cmd = app.update msg !state in
        state := new_state;
        process_cmd cmd)
  in

  process_cmd initial_cmd;

  (* Render thread: periodically re-renders if state changed *)
  let render_thread =
    Thread.create
      (fun () ->
        while !should_continue do
          let rendered = with_state (fun () -> render_el (app.view !state)) in
          if rendered <> !last_rendered
          then begin
            let rendered_lines = String.split_on_char '\n' rendered in
            let current_line_count = List.length rendered_lines in
            let max_line_width =
              List.fold_left
                (fun acc l -> max acc (visible_length l))
                0 rendered_lines
            in
            let block_pad =
              match options.alignment with
              | AlignLeft -> 0
              | AlignCenter -> max 0 ((term_width - max_line_width) / 2)
              | AlignRight -> max 0 (term_width - max_line_width)
            in
            let padding = String.make block_pad ' ' in
            let aligned_lines =
              if block_pad > 0
              then List.map (fun l -> padding ^ l) rendered_lines
              else rendered_lines
            in
            let buf = Buffer.create (String.length rendered + 256) in
            Buffer.add_string buf "\027[H";
            List.iter
              (fun l ->
                Buffer.add_string buf l;
                Buffer.add_string buf "\027[K\n")
              aligned_lines;
            for _ = 1 to max 0 (!last_line_count - current_line_count) do
              Buffer.add_string buf "\027[K\n"
            done;
            Stdlib.print_string (Buffer.contents buf);
            flush stdout;
            last_rendered := rendered;
            last_line_count := current_line_count
          end;
          Thread.delay (float_of_int options.render_interval_ms /. 1000.0)
        done)
      ()
  in

  (* Tick thread: fires timer subscriptions *)
  let last_tick_times = Hashtbl.create 4 in
  let tick_thread =
    Thread.create
      (fun () ->
        while !should_continue do
          let now = Unix.gettimeofday () *. 1000.0 in
          let tick_subs =
            with_state (fun () -> _get_tick_subs (app.subscriptions !state))
          in
          List.iter
            (fun (interval_ms, msg) ->
              let last_time =
                try Hashtbl.find last_tick_times interval_ms
                with Not_found -> 0.0
              in
              if now -. last_time >= float_of_int interval_ms
              then begin
                Hashtbl.replace last_tick_times interval_ms now;
                update_state msg
              end)
            tick_subs;
          Thread.delay 0.01
        done)
      ()
  in

  (* Input loop (main thread) *)
  let cleanup () =
    should_continue := false;
    (try Thread.join render_thread with _ -> ());
    (try Thread.join tick_thread with _ -> ());
    _show_cursor ();
    if options.clear_on_exit then _exit_alt_screen ();
    flush stdout;
    _exit_raw_mode old_attr
  in

  (try
     while !should_continue do
       let key = _read_key () in
       if key = options.quit_key
       then should_continue := false
       else begin
         let handlers =
           with_state (fun () -> _get_key_handlers (app.subscriptions !state))
         in
         List.iter
           (fun handler ->
             match handler key with
             | Some msg -> update_state msg
             | None -> ())
           handlers
       end
     done
   with _ -> ());
  cleanup (); !state

(** Run an interactive TUI application. *)
let run_app ?(options = default_options) app =
  ignore (_run_app_internal ~options app)

(** Run an interactive TUI application and return the final state. *)
let run_app_final ?(options = default_options) app =
  _run_app_internal ~options app
