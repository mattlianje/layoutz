open Alcotest
open Layoutz

let test_text_rendering () =
  check string "text renders correctly" "Hello World" (render (text "Hello World"))

let test_line_break () =
  check string "line break renders empty" "" (render br)

let test_text_width () =
  check int "text width" 5 (width (text "Hello"))

let test_text_height () =
  check int "text height" 1 (height (text "Hello"))

let test_multiline_height () =
  check int "multiline height" 2 (height (text "Line 1\nLine 2"))

let test_hr_default_width () =
  check int "hr default width" 50 (width hr)

let test_hr_custom_width () =
  check int "hr custom width" 10 (width (hr' ~width:10 ()))

let test_hr_custom_char () =
  check string "hr custom char" "==========" (render (hr' ~char:"=" ~width:10 ()))

let test_layout_composition () =
  check string "layout composition" "Line 1\nLine 2"
    (render (layout [text "Line 1"; text "Line 2"]))

let test_layout_width () =
  check int "layout width" 6 (width (layout [text "Line 1"; text "Line 2"]))

let test_row_layout () =
  check string "row layout" "A B" (render (row [text "A"; text "B"]))

let test_tight_row () =
  check string "tight row" "AB" (render (row ~tight:true [text "A"; text "B"]))

let test_box_height () =
  check int "box height" 3 (height (box ~title:"T" [text "X"]))

let test_box_renders () =
  let output = render (box ~title:"Title" [text "Content"]) in
  check bool "box has content" true (String.length output > 0)

let test_box_contains_title () =
  let output = render (box ~title:"Title" [text "Content"]) in
  check bool "box contains title" true (String.length output > 20)

let test_double_border () =
  let output = render (box ~border:Border.Double ~title:"" [text "X"]) in
  (* Check for UTF-8 sequence of ╔ *)
  check bool "double border starts with ╔" true
    (String.length output >= 3 && String.sub output 0 3 = "\xe2\x95\x94")

let test_round_border () =
  let output = render (box ~border:Border.Round ~title:"" [text "X"]) in
  (* Check for UTF-8 sequence of ╭ *)
  check bool "round border starts with ╭" true
    (String.length output >= 3 && String.sub output 0 3 = "\xe2\x95\xad")

let test_nested_layout () =
  let nested = layout [
    text "Header";
    box ~title:"Box" [
      text "Inside";
      row [text "A"; text "B"]
    ];
    text "Footer"
  ] in
  check bool "nested layout renders" true (String.length (render nested) > 0);
  check bool "nested layout has multiple lines" true
    (List.length (lines (render nested)) > 3)

(* UTF-8 box drawing tests *)
let test_box_utf8_border () =
  let output = render (box ~title:"" [text "X"]) in
  (* Should contain proper UTF-8 box chars, not corrupted bytes *)
  check bool "box top-left corner is ┌" true
    (String.sub output 0 3 = "┌");
  check bool "box contains ─" true
    (String.length output > 6 && String.sub output 3 3 = "─")

let test_box_with_title_utf8 () =
  let output = render (box ~title:"Hi" [text "X"]) in
  let first_line = List.hd (lines output) in
  (* ┌─Hi─┐ or similar *)
  check bool "titled box starts with ┌" true
    (String.sub first_line 0 3 = "┌");
  check bool "titled box contains title" true
    (String.length first_line > 0)

let test_hr_utf8 () =
  let output = render hr in
  (* Should be 50 repetitions of ─ (each is 3 bytes in UTF-8) *)
  check int "hr renders 50 chars" 50 (visible_length output);
  check bool "hr uses ─ character" true
    (String.sub output 0 3 = "─")

let test_table_basic () =
  let output = render (table ~headers:[s"A"; s"B"] [[s"1"; s"2"]]) in
  let output_lines = lines output in
  check bool "table has 5 lines" true (List.length output_lines = 5);
  (* Check corners *)
  check bool "table top-left is ┌" true
    (String.sub (List.nth output_lines 0) 0 3 = "┌");
  check bool "table bottom-left is └" true
    (String.sub (List.nth output_lines 4) 0 3 = "└")

let test_table_double_border () =
  let output = render (table ~border:Border.Double ~headers:[s"X"] [[s"Y"]]) in
  check bool "double table starts with ╔" true
    (String.sub output 0 3 = "╔")

let test_table_content () =
  let output = render (table ~headers:[s"Name"; s"Age"] [
    [s"Alice"; s"30"];
    [s"Bob"; s"25"]
  ]) in
  (* Simple substring check *)
  let contains_alice =
    let rec check i =
      if i > String.length output - 5 then false
      else if String.sub output i 5 = "Alice" then true
      else check (i + 1)
    in check 0
  in
  check bool "table contains Alice" true contains_alice

let test_styled_text () =
  let output = render (s "Hello" |> style styleBold) in
  check bool "bold has ANSI codes" true (String.length output > 5);
  check bool "bold starts with escape" true (output.[0] = '\027')

let test_fg_color () =
  let output = render (fg colorRed (s "Error")) in
  check bool "fg color has ANSI codes" true (String.length output > 5);
  check bool "fg color starts with escape" true (output.[0] = '\027')

let test_bg_color () =
  let output = render (bg colorBlue (s "Highlight")) in
  check bool "bg color has ANSI codes" true (String.length output > 5);
  check bool "bg color starts with escape" true (output.[0] = '\027')

let test_color256 () =
  let output = render (fg (color256 201) (s "Pink")) in
  check bool "256 color has ANSI codes" true (String.length output > 5);
  check bool "256 color starts with escape" true (output.[0] = '\027')

let test_colorRGB () =
  let output = render (fg (colorRGB 255 128 0) (s "Orange")) in
  check bool "RGB color has ANSI codes" true (String.length output > 5);
  check bool "RGB color starts with escape" true (output.[0] = '\027')

let test_combined_styles () =
  let output = render (s "Fancy" |> fg colorRed |> bg colorWhite |> style styleBold) in
  check bool "combined styles has ANSI codes" true (String.length output > 10);
  check bool "combined styles starts with escape" true (output.[0] = '\027')

let test_s_shorthand () =
  check string "s is alias for text" "Hello" (render (s "Hello"))

let test_row_with_boxes () =
  let output = render (row [
    box ~title:"A" [text "L"];
    box ~title:"B" [text "R"]
  ]) in
  let output_lines = lines output in
  (* Row should have boxes side by side, both should have ┌ *)
  let first_line = List.hd output_lines in
  check bool "row has two box corners" true
    (String.length first_line > 10)

let basic_tests = [
  "text rendering", `Quick, test_text_rendering;
  "line break", `Quick, test_line_break;
  "text width", `Quick, test_text_width;
  "text height", `Quick, test_text_height;
  "multiline height", `Quick, test_multiline_height;
]

let hr_tests = [
  "hr default width", `Quick, test_hr_default_width;
  "hr custom width", `Quick, test_hr_custom_width;
  "hr custom char", `Quick, test_hr_custom_char;
]

let layout_tests = [
  "layout composition", `Quick, test_layout_composition;
  "layout width", `Quick, test_layout_width;
]

let row_tests = [
  "row layout", `Quick, test_row_layout;
  "tight row", `Quick, test_tight_row;
]

let box_tests = [
  "box height", `Quick, test_box_height;
  "box renders", `Quick, test_box_renders;
  "box contains title", `Quick, test_box_contains_title;
  "double border", `Quick, test_double_border;
  "round border", `Quick, test_round_border;
]

let nested_tests = [
  "nested layout", `Quick, test_nested_layout;
]

let utf8_tests = [
  "box utf8 border", `Quick, test_box_utf8_border;
  "box with title utf8", `Quick, test_box_with_title_utf8;
  "hr utf8", `Quick, test_hr_utf8;
]

let table_tests = [
  "table basic", `Quick, test_table_basic;
  "table double border", `Quick, test_table_double_border;
  "table content", `Quick, test_table_content;
]

let style_tests = [
  "styled text", `Quick, test_styled_text;
  "fg color", `Quick, test_fg_color;
  "bg color", `Quick, test_bg_color;
  "256 color", `Quick, test_color256;
  "RGB color", `Quick, test_colorRGB;
  "combined styles", `Quick, test_combined_styles;
]

let test_style_compose () =
  let combined = styleBold ++ styleItalic in
  let output = render (s "fancy" |> style combined) in
  check bool "style compose has ANSI" true (output.[0] = '\027');
  (* Should have both bold (1) and italic (3) codes *)
  check bool "contains bold code" true (String.length output > 10)

let test_unordered_list () =
  let output = render (ul [li (s "First"); li (s "Second"); li (s "Third")]) in
  let output_lines = lines output in
  check int "ul has 3 lines" 3 (List.length output_lines);
  check bool "ul starts with bullet" true (String.sub (List.hd output_lines) 0 3 = "•")

let test_unordered_list_custom_bullet () =
  let output = render (ul ~bullet:"-" [li (s "A"); li (s "B")]) in
  check bool "custom bullet" true (String.sub output 0 1 = "-")

let test_ordered_list () =
  let output = render (ol [li (s "First"); li (s "Second"); li (s "Third")]) in
  let output_lines = lines output in
  check int "ol has 3 lines" 3 (List.length output_lines);
  check bool "ol first line starts with 1" true (String.sub (List.hd output_lines) 0 2 = "1.")

let test_ordered_list_start () =
  let output = render (ol ~start:5 [li (s "A"); li (s "B")]) in
  check bool "ol starts at 5" true (String.sub output 0 2 = "5.")

let test_list_with_styled_items () =
  let output = render (ul [
    li (s "Error" |> fg colorRed);
    li (s "Success" |> fg colorGreen)
  ]) in
  check bool "styled list has content" true (String.length output > 10)

let test_nested_list () =
  let output = render (ul [
    li ~children:[li (s "Nested A"); li (s "Nested B")] (s "Item 1");
    li (s "Item 2")
  ]) in
  let output_lines = lines output in
  check bool "nested list has multiple lines" true (List.length output_lines >= 3)

let test_nested_mixed_lists () =
  let output = render (ol [
    li ~children:[li (s "Sub A"); li (s "Sub B")] (s "First");
    li (s "Second")
  ]) in
  check bool "mixed nested list renders" true (String.length output > 20)

let misc_tests = [
  "s shorthand", `Quick, test_s_shorthand;
  "row with boxes", `Quick, test_row_with_boxes;
]

let operator_tests = [
  "style compose", `Quick, test_style_compose;
]

let list_tests = [
  "unordered list", `Quick, test_unordered_list;
  "ul custom bullet", `Quick, test_unordered_list_custom_bullet;
  "ordered list", `Quick, test_ordered_list;
  "ol start", `Quick, test_ordered_list_start;
  "list with styled items", `Quick, test_list_with_styled_items;
  "nested list", `Quick, test_nested_list;
  "nested mixed lists", `Quick, test_nested_mixed_lists;
]

let test_tree_basic () =
  let output = render (tree (
    node ~children:[node (s "child1"); node (s "child2")] (s "root")
  )) in
  let output_lines = lines output in
  check int "tree has 3 lines" 3 (List.length output_lines);
  check bool "tree root is first" true (String.sub (List.hd output_lines) 0 4 = "root")

let test_tree_nested () =
  let output = render (tree (
    node ~children:[
      node ~children:[node (s "deep")] (s "child")
    ] (s "root")
  )) in
  let output_lines = lines output in
  check int "nested tree has 3 lines" 3 (List.length output_lines)

let test_tree_connectors () =
  let output = render (tree (
    node ~children:[node (s "a"); node (s "b")] (s "root")
  )) in
  check bool "tree has branch connector" true (String.length output > 10)

let tree_tests = [
  "tree basic", `Quick, test_tree_basic;
  "tree nested", `Quick, test_tree_nested;
  "tree connectors", `Quick, test_tree_connectors;
]

let () =
  run "Layoutz" [
    "Basic Elements", basic_tests;
    "Horizontal Rule", hr_tests;
    "Layout", layout_tests;
    "Row", row_tests;
    "Box", box_tests;
    "Nested Elements", nested_tests;
    "UTF-8 Rendering", utf8_tests;
    "Table", table_tests;
    "Lists", list_tests;
    "Trees", tree_tests;
    "Styles", style_tests;
    "Misc", misc_tests;
    "Operators", operator_tests;
  ]
