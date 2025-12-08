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

(** The ELEMENT signature - implement this to create custom elements.

    Example:
    {[
      module MySpinner : Layoutz.ELEMENT = struct
        type t = { frame : int; label : string }
        let create ~frame ~label = { frame; label }
        let frames = [|"⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏"|]
        let render t = frames.(t.frame mod 10) ^ " " ^ t.label
        let width t = 2 + String.length t.label
        let height _ = 1
      end

      (* Use it: *)
      let spinner = Layoutz.el (module MySpinner) (MySpinner.create ~frame:0 ~label:"Loading")
    ]}
*)
module type ELEMENT = sig
  type t
  val render : t -> string
  val width : t -> int
  val height : t -> int
end

(** Existential wrapper - holds any element that satisfies ELEMENT *)
type element = E : (module ELEMENT with type t = 'a) * 'a -> element


(* ============================================================================
   Utility Functions
   ============================================================================ *)

(** Strip ANSI escape codes from a string for accurate width calculation *)
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let rec loop i in_escape =
    if i >= String.length s then Buffer.contents buf
    else
      let c = s.[i] in
      if in_escape then
        if c = 'm' then loop (i + 1) false
        else loop (i + 1) true
      else if c = '\027' && i + 1 < String.length s && s.[i + 1] = '[' then
        loop (i + 2) true
      else begin
        Buffer.add_char buf c;
        loop (i + 1) false
      end
  in
  loop 0 false

(** Count UTF-8 characters in a string *)
let utf8_length s =
  let rec count i acc =
    if i >= String.length s then acc
    else
      let c = Char.code s.[i] in
      if c land 0xC0 = 0x80 then count (i + 1) acc
      else count (i + 1) (acc + 1)
  in
  count 0 0

(** Calculate visible length of a string (ignoring ANSI codes, counting UTF-8 chars) *)
let visible_length s = utf8_length (strip_ansi s)

(** Split a string into lines *)
let lines s =
  let rec aux acc start i =
    if i >= String.length s then
      let last = String.sub s start (i - start) in
      List.rev (last :: acc)
    else if s.[i] = '\n' then
      let line = String.sub s start (i - start) in
      aux (line :: acc) (i + 1) (i + 1)
    else
      aux acc start (i + 1)
  in
  if s = "" then [""] else aux [] 0 0

(** Join lines with newlines *)
let unlines ls = String.concat "\n" ls

(** Pad a string to target width on the right *)
let pad_right target_width s =
  let len = visible_length s in
  if len >= target_width then s
  else s ^ String.make (target_width - len) ' '

(** Pad a string to target width on the left *)
let pad_left target_width s =
  let len = visible_length s in
  if len >= target_width then s
  else String.make (target_width - len) ' ' ^ s

(** Center a string within target width *)
let center_string target_width s =
  let len = visible_length s in
  if len >= target_width then s
  else
    let total_pad = target_width - len in
    let left_pad = total_pad / 2 in
    let right_pad = total_pad - left_pad in
    String.make left_pad ' ' ^ s ^ String.make right_pad ' '

(** Repeat a UTF-8 string n times *)
let repeat_str n s =
  String.concat "" (List.init n (fun _ -> s))


(* ============================================================================
   Core Operations on Elements
   ============================================================================ *)

let render_el (E ((module M), v)) = M.render v
let width_el (E ((module M), v)) = M.width v
let height_el (E ((module M), v)) = M.height v

(** Wrap any ELEMENT module + value into the universal element type *)
let el (type a) (module M : ELEMENT with type t = a) (v : a) : element =
  E ((module M), v)


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
    | RGB (r, g, b) -> "38;2;" ^ string_of_int r ^ ";" ^ string_of_int g ^ ";" ^ string_of_int b

  let bg_code = function
    | None -> ""
    | Code n -> string_of_int (n + 10)
    | C256 n -> "48;5;" ^ string_of_int n
    | RGB (r, g, b) -> "48;2;" ^ string_of_int r ^ ";" ^ string_of_int g ^ ";" ^ string_of_int b
end

(* Color values *)
type color = Color.t
let colorNone = Color.None
let colorBlack = Color.Code 30
let colorRed = Color.Code 31
let colorGreen = Color.Code 32
let colorYellow = Color.Code 33
let colorBlue = Color.Code 34
let colorMagenta = Color.Code 35
let colorCyan = Color.Code 36
let colorWhite = Color.Code 37
let colorBrightBlack = Color.Code 90
let colorBrightRed = Color.Code 91
let colorBrightGreen = Color.Code 92
let colorBrightYellow = Color.Code 93
let colorBrightBlue = Color.Code 94
let colorBrightMagenta = Color.Code 95
let colorBrightCyan = Color.Code 96
let colorBrightWhite = Color.Code 97
let color256 n = Color.C256 n
let colorRGB r g b = Color.RGB (r, g, b)

module Style = struct
  (** A style is a list of ANSI codes that can be composed *)
  type t = string list

  let none : t = []
  let codes (style : t) : string list = style
end

(* Style values *)
type style = Style.t
let styleBold : style = ["1"]
let styleDim : style = ["2"]
let styleItalic : style = ["3"]
let styleUnderline : style = ["4"]
let styleBlink : style = ["5"]
let styleReverse : style = ["7"]
let styleHidden : style = ["8"]
let styleStrikethrough : style = ["9"]

(** Compose two styles together *)
let ( ++ ) (a : style) (b : style) : style = a @ b

(** Styled element wrapper *)
module Styled = struct
  type t = {
    fg : Color.t;
    bg : Color.t;
    style : Style.t;
    inner : element;
  }

  let create ?(fg = Color.None) ?(bg = Color.None) ?(style = Style.none) inner =
    { fg; bg; style; inner }

  let render t =
    let codes =
      (if t.fg = Color.None then [] else [Color.fg_code t.fg]) @
      (if t.bg = Color.None then [] else [Color.bg_code t.bg]) @
      Style.codes t.style
    in
    if codes = [] then render_el t.inner
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
  type t = unit
  let create () = ()
  let render _ = ""
  let width _ = 0
  let height _ = 1
end

(** Horizontal rule *)
module HorizontalRule = struct
  type t = { char : string; rule_width : int }
  let create ?(char = "─") ?(rule_width = 50) () = { char; rule_width }
  let render t =
    let char_vis_len = visible_length t.char in
    if char_vis_len = 0 then String.make t.rule_width ' '
    else
      let repeats = t.rule_width / char_vis_len in
      String.concat "" (List.init repeats (fun _ -> t.char))
  let width t = t.rule_width
  let height _ = 1
end

(** VStack - vertical stacking *)
module VStack = struct
  type t = element list
  let create elements = elements
  let render t = String.concat "\n" (List.map render_el t)
  let width t = List.fold_left (fun acc e -> max acc (width_el e)) 0 t
  let height t = List.fold_left (fun acc e -> acc + height_el e) 0 t
end

(** HStack - horizontal arrangement *)
module HStack = struct
  type t = { elements : element list; tight : bool }
  let create ?(tight = false) elements = { elements; tight }
  let render t =
    if t.elements = [] then ""
    else
      let rendered = List.map render_el t.elements in
      let element_lines = List.map lines rendered in
      let max_height = List.fold_left (fun acc ls -> max acc (List.length ls)) 0 element_lines in
      let widths = List.map (fun ls -> List.fold_left (fun acc l -> max acc (visible_length l)) 0 ls) element_lines in
      let separator = if t.tight then "" else " " in
      let padded =
        List.map2 (fun w ls ->
          let padded_lines = List.map (pad_right w) ls in
          let missing = max_height - List.length ls in
          padded_lines @ List.init missing (fun _ -> String.make w ' ')
        ) widths element_lines
      in
      let transposed =
        List.init max_height (fun row ->
          String.concat separator (List.map (fun col -> List.nth col row) padded)
        )
      in
      unlines transposed
  let width t =
    let widths = List.map width_el t.elements in
    let total = List.fold_left ( + ) 0 widths in
    let gaps = if t.tight then 0 else max 0 (List.length t.elements - 1) in
    total + gaps
  let height t = List.fold_left (fun acc e -> max acc (height_el e)) 0 t.elements
end


(* ============================================================================
   Box / Container Elements
   ============================================================================ *)

module Border = struct
  type t = Normal | Double | Thick | Round | None

  let chars = function
    | Normal -> ("┌", "┐", "└", "┘", "─", "│")
    | Double -> ("╔", "╗", "╚", "╝", "═", "║")
    | Thick  -> ("┏", "┓", "┗", "┛", "━", "┃")
    | Round  -> ("╭", "╮", "╰", "╯", "─", "│")
    | None   -> (" ", " ", " ", " ", " ", " ")
end

(** Box - bordered container *)
module Box = struct
  type t = { title : string; elements : element list; border : Border.t }
  let create ?(border = Border.Normal) ~title elements = { title; elements; border }
  let render t =
    let content = String.concat "\n" (List.map render_el t.elements) in
    let content_lines = if content = "" then [""] else lines content in
    let content_width = List.fold_left (fun acc l -> max acc (visible_length l)) 0 content_lines in
    let title_width = if t.title = "" then 0 else visible_length t.title + 2 in
    let inner_width = max content_width title_width in
    let total_width = inner_width + 4 in
    let (tl, tr, bl, br, h, v) = Border.chars t.border in
    let top_border =
      if t.title = "" then
        tl ^ repeat_str (total_width - 2) h ^ tr
      else
        let title_padding = total_width - visible_length t.title - 2 in
        let left_pad = title_padding / 2 in
        let right_pad = title_padding - left_pad in
        tl ^ repeat_str left_pad h ^ t.title ^ repeat_str right_pad h ^ tr
    in
    let bottom_border = bl ^ repeat_str (total_width - 2) h ^ br in
    let padded_content =
      List.map (fun line -> v ^ " " ^ pad_right inner_width line ^ " " ^ v) content_lines
    in
    unlines (top_border :: padded_content @ [bottom_border])
  let width t =
    let content = String.concat "\n" (List.map render_el t.elements) in
    let content_lines = if content = "" then [""] else lines content in
    let content_width = List.fold_left (fun acc l -> max acc (visible_length l)) 0 content_lines in
    let title_width = if t.title = "" then 0 else visible_length t.title + 2 in
    max content_width title_width + 4
  let height t =
    let content = String.concat "\n" (List.map render_el t.elements) in
    let content_lines = if content = "" then [""] else lines content in
    List.length content_lines + 2
end

(** Table - bordered table with element cells *)
module Table = struct
  type t = { headers : element list; rows : element list list; border : Border.t }
  let create ?(border = Border.Normal) ~headers rows = { headers; rows; border }
  let render t =
    let num_cols = List.length t.headers in
    let normalize_row row =
      let len = List.length row in
      if len >= num_cols then List.filteri (fun i _ -> i < num_cols) row
      else row @ List.init (num_cols - len) (fun _ -> el (module Text) "")
    in
    let header_strs = List.map render_el t.headers in
    let rendered_rows = List.map (fun row -> List.map render_el (normalize_row row)) t.rows in
    let col_widths =
      let header_widths = List.map visible_length header_strs in
      let row_widths = List.map (fun row ->
        List.map (fun cell ->
          List.fold_left (fun acc l -> max acc (visible_length l)) 0 (lines cell)
        ) row
      ) rendered_rows in
      List.mapi (fun i hw ->
        List.fold_left (fun acc row -> max acc (List.nth row i)) hw row_widths
      ) header_widths
    in
    let (tl, tr, bl, br, h, v) = Border.chars t.border in
    let top_conn = match t.border with
      | Border.Round | Border.Normal -> "┬" | Border.Double -> "╦"
      | Border.Thick -> "┳" | Border.None -> " "
    in
    let mid_conn = match t.border with
      | Border.Round | Border.Normal -> "┼" | Border.Double -> "╬"
      | Border.Thick -> "╋" | Border.None -> " "
    in
    let bot_conn = match t.border with
      | Border.Round | Border.Normal -> "┴" | Border.Double -> "╩"
      | Border.Thick -> "┻" | Border.None -> " "
    in
    let left_tee = match t.border with
      | Border.Round | Border.Normal -> "├" | Border.Double -> "╠"
      | Border.Thick -> "┣" | Border.None -> " "
    in
    let right_tee = match t.border with
      | Border.Round | Border.Normal -> "┤" | Border.Double -> "╣"
      | Border.Thick -> "┫" | Border.None -> " "
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
    let data_rows = List.map (fun row ->
      let cell_lines = List.map lines row in
      let max_h = List.fold_left (fun acc ls -> max acc (List.length ls)) 1 cell_lines in
      let padded_cells = List.map2 (fun w cls ->
        let padded = List.map (pad_right w) cls in
        padded @ List.init (max_h - List.length cls) (fun _ -> String.make w ' ')
      ) col_widths cell_lines in
      let transposed = List.init max_h (fun r ->
        List.map (fun col -> List.nth col r) padded_cells
      ) in
      List.map (fun row_cells ->
        v ^ " " ^ String.concat (" " ^ v ^ " ") row_cells ^ " " ^ v
      ) transposed
    ) rendered_rows in
    unlines ([top_border; header_row; sep_border] @ List.concat data_rows @ [bot_border])
  let width t =
    let col_widths = List.map width_el t.headers in
    let total = List.fold_left ( + ) 0 col_widths in
    total + (List.length col_widths * 3) + 1
  let height t =
    let row_heights = List.map (fun row ->
      List.fold_left (fun acc cell -> max acc (height_el cell)) 1 row
    ) t.rows in
    3 + List.fold_left ( + ) 0 row_heights
end

(** List item type - recursive for deep nesting *)
type list_item = { content : element; children : list_item list }

(** Tree node type - recursive *)
type tree_node = { label : element; branches : tree_node list }

(** Unordered list - items are (element, children) pairs *)
module UList = struct
  type t = { items : list_item list; bullet : string }

  (* Rotating bullets for nested levels *)
  let bullets = [|"•"; "◦"; "▪"; "▫"|]
  let bullet_for_level level = bullets.(level mod Array.length bullets)

  let create ?(bullet = "•") items = { items; bullet }

  let rec render_with_indent indent level t =
    let prefix_indent = String.make indent ' ' in
    let bullet = if level = 0 then t.bullet else bullet_for_level level in
    let bullet_width = visible_length bullet + 1 in
    let format_item item =
      let rendered = render_el item.content in
      let item_lines = lines rendered in
      let first_part = match item_lines with
        | [] -> prefix_indent ^ bullet ^ " "
        | first :: rest ->
          let first_line = prefix_indent ^ bullet ^ " " ^ first in
          let cont_indent = String.make (indent + bullet_width) ' ' in
          let rest_lines = List.map (fun l -> cont_indent ^ l) rest in
          unlines (first_line :: rest_lines)
      in
      if item.children = [] then first_part
      else
        let child_list = { items = item.children; bullet = t.bullet } in
        let child_rendered = render_with_indent (indent + 2) (level + 1) child_list in
        first_part ^ "\n" ^ child_rendered
    in
    String.concat "\n" (List.map format_item t.items)

  let render t = render_with_indent 0 0 t
  let width t =
    let bullet_width = visible_length t.bullet + 1 in
    let max_item_width = List.fold_left (fun acc item -> max acc (width_el item.content)) 0 t.items in
    bullet_width + max_item_width
  let height t =
    List.fold_left (fun acc item -> acc + height_el item.content + List.length item.children) 0 t.items
end

(** Ordered list - items are (element, children) pairs *)
module OList = struct
  type t = { items : list_item list; start : int }

  (* Convert number to letter (a, b, c, ..., z, aa, ab, ...) *)
  let to_letter n =
    let rec aux acc n =
      if n < 0 then acc
      else aux (String.make 1 (Char.chr (97 + (n mod 26))) ^ acc) (n / 26 - 1)
    in
    aux "" n

  (* Convert number to roman numeral *)
  let to_roman n =
    let numerals = [
      (1000, "m"); (900, "cm"); (500, "d"); (400, "cd");
      (100, "c"); (90, "xc"); (50, "l"); (40, "xl");
      (10, "x"); (9, "ix"); (5, "v"); (4, "iv"); (1, "i")
    ] in
    let rec aux acc n = function
      | [] -> acc
      | (value, numeral) :: rest ->
        if n >= value then aux (acc ^ numeral) (n - value) ((value, numeral) :: rest)
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
    let num_width = List.fold_left (fun acc s -> max acc (String.length s)) 0 sample_nums in
    let format_item idx item =
      let num_str = format_num level idx in
      let padded_num = String.make (num_width - String.length num_str) ' ' ^ num_str in
      let prefix = padded_num ^ ". " in
      let prefix_width = num_width + 2 in
      let rendered = render_el item.content in
      let item_lines = lines rendered in
      let indent_str = String.make prefix_indent ' ' in
      let first_part = match item_lines with
        | [] -> indent_str ^ prefix
        | first :: rest ->
          let first_line = indent_str ^ prefix ^ first in
          let cont_indent = String.make (prefix_indent + prefix_width) ' ' in
          let rest_lines = List.map (fun l -> cont_indent ^ l) rest in
          unlines (first_line :: rest_lines)
      in
      if item.children = [] then first_part
      else
        let child_rendered = render_with_indent (prefix_indent + prefix_width) (level + 1) 1 item.children in
        first_part ^ "\n" ^ child_rendered
    in
    String.concat "\n" (List.mapi format_item items)

  let render t = render_with_indent 0 0 t.start t.items
  let width t =
    let num_items = List.length t.items in
    let max_num = t.start + num_items - 1 in
    let num_width = String.length (string_of_int max_num) + 2 in
    let max_item_width = List.fold_left (fun acc item -> max acc (width_el item.content)) 0 t.items in
    num_width + max_item_width
  let height t =
    List.fold_left (fun acc item -> acc + height_el item.content + List.length item.children) 0 t.items
end

(** Tree - renders with box-drawing connectors *)
module Tree = struct
  type t = tree_node

  let create node = node

  let rec render_node prefix is_last node =
    let connector = if is_last then "└── " else "├── " in
    let rendered = render_el node.label in
    let label_lines = lines rendered in
    let first_line = prefix ^ connector ^ (match label_lines with [] -> "" | h :: _ -> h) in
    let child_prefix = prefix ^ (if is_last then "    " else "│   ") in
    let rest_lines = match label_lines with
      | [] | [_] -> []
      | _ :: rest -> List.map (fun l -> child_prefix ^ l) rest
    in
    let num_children = List.length node.branches in
    let children_rendered = List.mapi (fun i child ->
      render_node child_prefix (i = num_children - 1) child
    ) node.branches in
    String.concat "\n" ([first_line] @ rest_lines @ children_rendered)

  let render t =
    let rendered = render_el t.label in
    let label_lines = lines rendered in
    let root_line = match label_lines with [] -> "" | h :: _ -> h in
    let rest_lines = match label_lines with [] | [_] -> [] | _ :: rest -> rest in
    let num_children = List.length t.branches in
    let children_rendered = List.mapi (fun i child ->
      render_node "" (i = num_children - 1) child
    ) t.branches in
    String.concat "\n" ([root_line] @ rest_lines @ children_rendered)

  let rec node_width node =
    let label_w = width_el node.label in
    let children_w = List.fold_left (fun acc child -> max acc (4 + node_width child)) 0 node.branches in
    max label_w children_w

  let width t = node_width t

  let rec node_height node =
    1 + List.fold_left (fun acc child -> acc + node_height child) 0 node.branches

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

(** Horizontal rule (default: 50 wide with ─) *)
let hr = el (module HorizontalRule) (HorizontalRule.create ())

(** Horizontal rule with options *)
let hr' ?(char = "─") ?(width = 50) () =
  el (module HorizontalRule) (HorizontalRule.create ~char ~rule_width:width ())

(** Vertical stack *)
let vstack elements = el (module VStack) (VStack.create elements)

(** Horizontal stack *)
let hstack ?(tight = false) elements = el (module HStack) (HStack.create ~tight elements)

(** Bordered box *)
let box ?(border = Border.Normal) ~title elements =
  el (module Box) (Box.create ~border ~title elements)

(** Table with element headers and rows *)
let table ?(border = Border.Normal) ~headers rows =
  el (module Table) (Table.create ~border ~headers rows)

(** List item - can have nested children (which are also list_items) *)
let li ?(children : list_item list = []) item : list_item = { content = item; children }

(** Unordered list - takes elements or li items *)
let ul ?(bullet = "•") items =
  el (module UList) (UList.create ~bullet items)

(** Ordered list - takes elements or li items *)
let ol ?(start = 1) items =
  el (module OList) (OList.create ~start items)

(** Tree node constructor *)
let node ?(children = []) label : tree_node = { label; branches = children }

(** Tree element *)
let tree root =
  el (module Tree) (Tree.create root)

(** Style an element with colors and style *)
let styled ?(fg = colorNone) ?(bg = colorNone) ?(style = Style.none) inner =
  el (module Styled) (Styled.create ~fg ~bg ~style inner)

(** Styling functions - pipe-friendly (element last) *)
let style st e = styled ~style:st e
let fg color e = styled ~fg:color e
let bg color e = styled ~bg:color e

(** Render element to string *)
let render = render_el

(** Get element width *)
let width = width_el

(** Get element height *)
let height = height_el

(** Print element to stdout *)
let put_str_ln element = print_endline (render element)

(* Legacy aliases *)
let layout = vstack
let row = hstack
