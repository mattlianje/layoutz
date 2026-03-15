open Layoutz

(* ========================================================================
   State
   ======================================================================== *)

type showcase_state = {
  scene: int;
  tick: int;
  text_value: string;
  items: string list;
  adding_item: bool;
  add_tick: int;
  selected: int list;
  cursor: int;
  line_offset: int;
  table_row: int;
  table_selected: int list;
  bar_mode: int;
  ball_y: float;
  ball_vy: float;
  gravity: int;
  ball_trail: float list;
}

let initial_state =
  {
    scene = 0;
    tick = 0;
    text_value = "";
    items = [];
    adding_item = false;
    add_tick = 0;
    selected = [];
    cursor = 0;
    line_offset = 0;
    table_row = 0;
    table_selected = [];
    bar_mode = 0;
    ball_y = 10.0;
    ball_vy = 0.0;
    gravity = 5;
    ball_trail = List.init 80 (fun _ -> 10.0);
  }

(* ========================================================================
   Messages
   ======================================================================== *)

type msg =
  | NextScene
  | PrevScene
  | GoScene of int
  | Tick
  | TypeChar of char
  | Backspace
  | SubmitItem
  | ToggleSelect
  | CursorUp
  | CursorDown
  | AdjustUp
  | AdjustDown
  | ToggleBarMode
  | KickBall

(* ========================================================================
   Constants
   ======================================================================== *)

let total_scenes = 7

let scene_names =
  [|
    "Physics Game";
    "Text Input & Lists";
    "Borders & Styles";
    "Tables";
    "Charts & Plots";
    "Bar Charts & Sparklines";
    "Selections & Heatmap";
  |]

let services =
  [|
    ("API Gateway", "LIVE", "12ms", "99.99%");
    ("Database", "LIVE", "3ms", "99.95%");
    ("Cache", "WARN", "1ms", "98.50%");
    ("Queue", "LIVE", "8ms", "99.90%");
    ("Auth", "LIVE", "5ms", "99.97%");
    ("CDN", "LIVE", "2ms", "99.99%");
  |]

let scene_width = 75

(* ========================================================================
   Helpers
   ======================================================================== *)

let toggle_in x xs =
  if List.mem x xs then List.filter (fun i -> i <> x) xs else x :: xs

let take_right n lst =
  let len = List.length lst in
  if len <= n then lst else List.filteri (fun i _ -> i >= len - n) lst

(* ========================================================================
   Update
   ======================================================================== *)

let bounce y vy =
  if y <= 0.0
  then (0.0, Float.abs vy *. 0.82)
  else if y > 12.0
  then (12.0, -.(Float.abs vy *. 0.5))
  else if Float.abs vy < 0.05 && y < 0.1
  then (0.0, 0.0)
  else (y, vy)

let update msg st =
  match msg with
  | NextScene -> ({ st with scene = (st.scene + 1) mod total_scenes }, CmdNone)
  | PrevScene ->
    ({ st with scene = (st.scene - 1 + total_scenes) mod total_scenes }, CmdNone)
  | GoScene n when n >= 0 && n < total_scenes -> ({ st with scene = n }, CmdNone)
  | GoScene _ -> (st, CmdNone)
  | Tick ->
    (* Text input adding animation *)
    let s1 =
      if st.adding_item && st.add_tick >= 8
      then
        {
          st with
          adding_item = false;
          add_tick = 0;
          items = st.items @ [ st.text_value ];
          text_value = "";
        }
      else if st.adding_item
      then { st with add_tick = st.add_tick + 1 }
      else st
    in
    (* Ball physics *)
    let g = float_of_int s1.gravity *. 0.08 in
    let new_vy = s1.ball_vy -. g in
    let raw_y = s1.ball_y +. (new_vy *. 0.3) in
    let ny, vy = bounce raw_y new_vy in
    let trail = take_right 80 (s1.ball_trail @ [ ny ]) in
    ( {
        s1 with
        tick = s1.tick + 1;
        ball_y = ny;
        ball_vy = vy;
        ball_trail = trail;
      },
      CmdNone )
  | TypeChar c when not st.adding_item ->
    ({ st with text_value = st.text_value ^ String.make 1 c }, CmdNone)
  | TypeChar _ -> (st, CmdNone)
  | Backspace when (not st.adding_item) && String.length st.text_value > 0 ->
    ( {
        st with
        text_value = String.sub st.text_value 0 (String.length st.text_value - 1);
      },
      CmdNone )
  | Backspace -> (st, CmdNone)
  | SubmitItem when (not st.adding_item) && st.text_value <> "" ->
    ({ st with adding_item = true; add_tick = 0 }, CmdNone)
  | SubmitItem -> (st, CmdNone)
  | ToggleSelect ->
    if st.scene = 3
    then
      ( { st with table_selected = toggle_in st.table_row st.table_selected },
        CmdNone )
    else if st.scene = 6
    then ({ st with selected = toggle_in st.cursor st.selected }, CmdNone)
    else (st, CmdNone)
  | CursorUp ->
    if st.scene = 3
    then
      ( {
          st with
          table_row =
            (st.table_row - 1 + Array.length services) mod Array.length services;
        },
        CmdNone )
    else if st.scene = 6
    then ({ st with cursor = (st.cursor - 1 + 7) mod 7 }, CmdNone)
    else (st, CmdNone)
  | CursorDown ->
    if st.scene = 3
    then
      ( { st with table_row = (st.table_row + 1) mod Array.length services },
        CmdNone )
    else if st.scene = 6
    then ({ st with cursor = (st.cursor + 1) mod 7 }, CmdNone)
    else (st, CmdNone)
  | AdjustUp ->
    if st.scene = 0
    then ({ st with gravity = min (st.gravity + 1) 15 }, CmdNone)
    else ({ st with line_offset = min (st.line_offset + 1) 10 }, CmdNone)
  | AdjustDown ->
    if st.scene = 0
    then ({ st with gravity = max (st.gravity - 1) 1 }, CmdNone)
    else ({ st with line_offset = max (st.line_offset - 1) (-10) }, CmdNone)
  | ToggleBarMode -> ({ st with bar_mode = (st.bar_mode + 1) mod 2 }, CmdNone)
  | KickBall -> ({ st with ball_vy = 5.0 }, CmdNone)

(* ========================================================================
   Subscriptions
   ======================================================================== *)

let subscriptions st =
  sub_batch
    [
      sub_every_ms 80 Tick;
      sub_key_press (fun key ->
          match key with
          | KeyRight -> Some NextScene
          | KeyLeft -> Some PrevScene
          | KeyChar '+' -> Some AdjustUp
          | KeyChar '-' -> Some AdjustDown
          | KeyChar ' ' when st.scene = 0 -> Some KickBall
          | KeyChar ' ' when st.scene = 3 || st.scene = 6 -> Some ToggleSelect
          | KeyTab when st.scene = 5 -> Some ToggleBarMode
          | KeyEnter when st.scene = 1 -> Some SubmitItem
          | KeyUp -> Some CursorUp
          | KeyDown -> Some CursorDown
          | KeyBackspace -> Some Backspace
          | KeyChar c
            when st.scene = 1
                 && ((c >= 'a' && c <= 'z')
                    || (c >= 'A' && c <= 'Z')
                    || (c >= '0' && c <= '9')
                    || c = ' ') ->
            Some (TypeChar c)
          | KeyChar c when c >= '1' && c <= '7' ->
            Some (GoScene (Char.code c - Char.code '1'))
          | _ -> None);
    ]

(* ========================================================================
   View - Header & Footer
   ======================================================================== *)

let render_header st =
  let scene_dots =
    String.concat " "
      (List.init total_scenes (fun i -> if i = st.scene then "●" else "○"))
  in
  let prefix = " ─── " in
  let title = "layoutz" in
  let suffix = Printf.sprintf "%d / %d" (st.scene + 1) total_scenes in
  let dash_count =
    max 3
      (scene_width
      - visible_length prefix
      - String.length title
      - String.length suffix
      - 2)
  in
  let dashes = String.concat "" (List.init dash_count (fun _ -> "─")) in
  layout
    [
      br;
      tightRow
        [
          s " ─── " |> colorBrightBlack;
          s title |> styleBold ++ colorBrightCyan;
          s (Printf.sprintf " %s " dashes) |> colorBrightBlack;
          s suffix |> colorBrightBlack;
        ];
      br;
      s (Printf.sprintf " %s" scene_names.(st.scene))
      |> styleBold ++ colorBrightYellow;
      s (Printf.sprintf " %s" scene_dots);
    ]

let render_footer st =
  let hints =
    match st.scene with
    | 0 -> "  </> scenes  Space kick  +/- gravity  Ctrl+Q quit"
    | 1 -> "  </> scenes  type + Enter to add  Ctrl+Q quit"
    | 3 -> "  </> scenes  ^/v navigate  Space select  Ctrl+Q quit"
    | 4 -> "  </> scenes  +/- move threshold  Ctrl+Q quit"
    | 5 -> "  </> scenes  Tab cycle chart mode  Ctrl+Q quit"
    | 6 -> "  </> scenes  ^/v navigate  Space toggle  Ctrl+Q quit"
    | _ -> "  </> scenes  Ctrl+Q quit"
  in
  s hints |> styleDim ++ colorBrightBlack

(* ========================================================================
   Scene 1: Physics Game
   ======================================================================== *)

let scene_physics_game st =
  let trail_points = List.mapi (fun i y -> (float_of_int i, y)) st.ball_trail in
  let g_label = Printf.sprintf "g = %.2f" (float_of_int st.gravity *. 0.08) in
  let vel_label = Printf.sprintf "vy = %.1f" st.ball_vy in
  let y_label = Printf.sprintf "y = %.1f" st.ball_y in
  let bounds = [ (0.0, 0.0); (0.0, 12.0) ] in
  let energy = min 1.0 ((Float.abs st.ball_vy +. st.ball_y) /. 15.0) in
  let bar_w = 14 in
  let filled = int_of_float (energy *. float_of_int bar_w) in
  let pct = int_of_float (energy *. 100.0) in
  let energy_bar =
    Printf.sprintf "Energy %s%s %d%%"
      (String.concat "" (List.init filled (fun _ -> "█")))
      (String.concat "" (List.init (bar_w - filled) (fun _ -> "░")))
      pct
  in
  columns
    [
      layout
        [
          s "Trajectory" |> colorBrightYellow;
          plotLine ~width:35 ~height:12
            [
              series ~points:trail_points ~label:"ball" ~color:Color.brightCyan;
              series ~points:bounds ~label:" " ~color:Color.brightBlack;
            ];
        ];
      box ~title:"Physics"
        [
          left_align ~width:28
            (layout
               [
                 kv
                   [
                     ("gravity", g_label);
                     ("velocity", vel_label);
                     ("height", y_label);
                   ];
                 br;
                 s energy_bar |> colorBrightGreen;
                 br;
                 spinner ~label:"Simulating" ~frame:(st.tick / 3)
                   ~style:SpinnerStyle.Earth
                 |> colorBrightCyan;
                 s "Press Space to kick ball!" |> styleBold ++ colorBrightYellow;
               ]);
        ]
      |> borderRound
      |> colorBrightMagenta;
    ]

(* ========================================================================
   Scene 2: Text Input & Lists
   ======================================================================== *)

let scene_text_input st =
  let input_line =
    if st.adding_item
    then
      row
        [
          spinner ~label:"Adding" ~frame:st.add_tick ~style:SpinnerStyle.Dots
          |> colorBrightYellow;
          s (Printf.sprintf "  \"%s\"" st.text_value) |> colorBrightYellow;
        ]
    else
      let display =
        if st.text_value = ""
        then s "Type something..." |> colorBrightBlack
        else s st.text_value |> colorBrightWhite
      in
      tightRow [ s "> " |> colorBrightCyan; display; s "_" |> styleBlink ]
  in
  let item_colors =
    [|
      colorBrightGreen;
      colorBrightBlue;
      colorBrightMagenta;
      colorBrightYellow;
      colorBrightCyan;
    |]
  in
  let item_list =
    if st.items = []
    then s "  (no items yet)" |> colorBrightBlack
    else
      layout
        (List.mapi
           (fun i item ->
             tightRow
               [
                 s (Printf.sprintf "  %d. " (i + 1)) |> colorBrightBlack;
                 s item |> item_colors.(i mod Array.length item_colors);
               ])
           st.items)
  in
  let box_w = 32 in
  let its = st.items in
  let cnt = List.length its in
  let longest =
    if its = []
    then "-"
    else
      List.fold_left
        (fun a b -> if String.length a >= String.length b then a else b)
        "" its
  in
  let shortest =
    if its = []
    then "-"
    else
      List.fold_left
        (fun a b -> if String.length a <= String.length b then a else b)
        (List.hd its) its
  in
  columns
    [
      box ~title:"Add Items"
        [
          left_align ~width:box_w input_line;
          br;
          left_align ~width:box_w
            (layout [ s "Items:" |> styleBold; item_list ]);
        ]
      |> borderRound
      |> colorBrightCyan;
      box ~title:"Stats"
        [
          left_align ~width:box_w
            (layout
               [
                 tightRow
                   [
                     s "Total items: ";
                     s (string_of_int cnt) |> styleBold ++ colorBrightCyan;
                   ];
                 tightRow [ s "Longest:     "; s longest |> colorBrightMagenta ];
                 tightRow
                   [ s "Shortest:    "; s shortest |> colorBrightMagenta ];
               ]);
          br;
          left_align ~width:box_w
            (if cnt >= 3
             then s "Nice collection!" |> styleBold ++ colorBrightGreen
             else
               s (Printf.sprintf "Add %d more..." (3 - cnt)) |> colorBrightBlack);
        ]
      |> borderRound;
    ]

(* ========================================================================
   Scene 3: Borders & Styles
   ======================================================================== *)

let scene_borders_styles _st =
  let mk_box bdr name color_fn =
    box ~title:name [ left_align ~width:8 (s name) ] |> bdr |> color_fn
  in
  layout
    [
      s "Border Styles" |> styleBold ++ colorBrightYellow;
      row
        [
          mk_box borderNormal "Single" colorBrightCyan;
          mk_box borderDouble "Double" colorBrightMagenta;
          mk_box borderRound "Round" colorBrightGreen;
        ];
      row
        [
          mk_box borderThick "Thick" colorBrightYellow;
          mk_box borderDashed "Dashed" colorBrightBlue;
          mk_box borderAscii "Ascii" colorBrightWhite;
        ];
      br;
      s "Text Styles" |> styleBold ++ colorBrightYellow;
      row
        [
          box ~title:"Standard"
            [
              s "Bold" |> styleBold ++ colorBrightCyan;
              s "Italic" |> styleItalic ++ colorBrightMagenta;
              s "Underline" |> styleUnderline ++ colorBrightGreen;
            ]
          |> borderRound;
          box ~title:"Extended"
            [
              s "Dim" |> styleDim ++ colorBrightYellow;
              s "Strikethrough" |> styleStrikethrough ++ colorBrightRed;
              s "Bold+Italic" |> styleBold ++ styleItalic ++ colorBrightWhite;
            ]
          |> borderRound;
        ];
    ]

(* ========================================================================
   Scene 4: Tables
   ======================================================================== *)

let scene_tables st =
  let n_services = Array.length services in
  let colored_rows =
    List.init n_services (fun idx ->
        let name, status, lat, up = services.(idx) in
        let is_active = idx = st.table_row in
        let is_sel = List.mem idx st.table_selected in
        let mark = if is_sel then "* " else "  " in
        let cells = [ mark ^ name; status; lat; up ] in
        List.map
          (fun cell ->
            if is_active && is_sel
            then s cell |> styleBold ++ styleReverse ++ colorBrightGreen
            else if is_active
            then s cell |> styleBold ++ styleReverse ++ colorBrightCyan
            else if is_sel
            then s cell |> colorBrightGreen
            else s cell)
          cells)
  in
  let sel_count = List.length st.table_selected in
  let sel_info =
    if sel_count > 0
    then s (Printf.sprintf "%d selected" sel_count) |> colorBrightGreen
    else s "none selected" |> colorBrightBlack
  in
  layout
    [
      table
        ~headers:
          [
            s "Service" |> styleBold;
            s "Status" |> styleBold;
            s "Latency" |> styleBold;
            s "Uptime" |> styleBold;
          ]
        colored_rows
      |> borderRound;
      tightRow
        [
          s (Printf.sprintf " Row %d/%d  |  " (st.table_row + 1) n_services)
          |> colorBrightBlack;
          sel_info;
        ];
    ]

(* ========================================================================
   Scene 5: Charts & Plots
   ======================================================================== *)

let scene_charts_plots st =
  let sin_points =
    List.init 101 (fun i ->
        let x = float_of_int i *. 0.08 in
        (x, sin (x +. (float_of_int st.tick *. 0.06)) *. 4.0))
  in
  let intercept = float_of_int st.line_offset *. 0.5 in
  let line_points =
    List.init 101 (fun i ->
        let x = float_of_int i *. 0.08 in
        (x, (0.5 *. x) +. intercept))
  in
  let sign = if intercept >= 0.0 then "+" else "-" in
  let line_label = Printf.sprintf "0.5x %s %.1f" sign (Float.abs intercept) in
  columns
    [
      layout
        [
          s (Printf.sprintf "sin(x) & y = %s  [+/- to shift]" line_label)
          |> colorBrightYellow;
          plotLine ~width:35 ~height:12
            [
              series ~points:sin_points ~label:"sin(x)" ~color:Color.brightCyan;
              series ~points:line_points ~label:"linear"
                ~color:Color.brightYellow;
            ];
        ];
      layout
        [
          s "Revenue Share" |> colorBrightYellow;
          plotPie ~width:30 ~height:8
            [
              slice ~value:45.0 ~label:"Product" ~color:Color.brightCyan;
              slice ~value:30.0 ~label:"Services" ~color:Color.brightMagenta;
              slice ~value:15.0 ~label:"Licensing" ~color:Color.brightYellow;
              slice ~value:10.0 ~label:"Other" ~color:Color.brightGreen;
            ];
        ];
    ]

(* ========================================================================
   Scene 6: Bar Charts & Sparklines
   ======================================================================== *)

let scene_bar_charts_sparklines st =
  let spark_data =
    List.init 30 (fun i ->
        (sin (float_of_int (i + st.tick) *. 0.3) *. 10.0) +. 15.0)
  in
  let mode_name = if st.bar_mode = 0 then "Vertical Bars" else "Stacked Bars" in
  let chart_element =
    if st.bar_mode = 0
    then
      plotBar ~width:30 ~height:8
        [
          bar_item ~value:85.0 ~label:"Mon" ~color:Color.brightCyan;
          bar_item ~value:120.0 ~label:"Tue" ~color:Color.brightGreen;
          bar_item ~value:95.0 ~label:"Wed" ~color:Color.brightMagenta;
          bar_item ~value:110.0 ~label:"Thu" ~color:Color.brightYellow;
          bar_item ~value:75.0 ~label:"Fri" ~color:Color.brightBlue;
        ]
    else
      plotStackedBar ~width:30 ~height:8
        [
          stacked_group
            ~segments:
              [
                bar_item ~value:50.0 ~label:"Online" ~color:Color.brightCyan;
                bar_item ~value:35.0 ~label:"Retail" ~color:Color.brightGreen;
                bar_item ~value:15.0 ~label:"Other" ~color:Color.brightMagenta;
              ]
            ~label:"Q1";
          stacked_group
            ~segments:
              [
                bar_item ~value:70.0 ~label:"Online" ~color:Color.brightCyan;
                bar_item ~value:30.0 ~label:"Retail" ~color:Color.brightGreen;
                bar_item ~value:20.0 ~label:"Other" ~color:Color.brightMagenta;
              ]
            ~label:"Q2";
          stacked_group
            ~segments:
              [
                bar_item ~value:45.0 ~label:"Online" ~color:Color.brightCyan;
                bar_item ~value:55.0 ~label:"Retail" ~color:Color.brightGreen;
                bar_item ~value:10.0 ~label:"Other" ~color:Color.brightMagenta;
              ]
            ~label:"Q3";
          stacked_group
            ~segments:
              [
                bar_item ~value:60.0 ~label:"Online" ~color:Color.brightCyan;
                bar_item ~value:40.0 ~label:"Retail" ~color:Color.brightGreen;
                bar_item ~value:25.0 ~label:"Other" ~color:Color.brightMagenta;
              ]
            ~label:"Q4";
        ]
  in
  columns
    [
      layout
        [
          s "Live Signal" |> colorBrightYellow;
          sparkline spark_data |> colorBrightCyan;
        ];
      layout
        [
          s (Printf.sprintf "%s  [Tab to cycle]" mode_name) |> colorBrightYellow;
          chart_element;
        ];
    ]

(* ========================================================================
   Scene 7: Selections & Heatmap
   ======================================================================== *)

let scene_selections_heatmap st =
  let days = [| "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" |] in
  let hours = [ "6am"; "9am"; "12pm"; "3pm"; "6pm"; "9pm" ] in
  let selector_lines =
    List.init 7 (fun idx ->
        let day = days.(idx) in
        let is_sel = List.mem idx st.selected in
        let is_cur = st.cursor = idx in
        let check = if is_sel then "[x]" else "[ ]" in
        let arrow = if is_cur then "> " else "  " in
        let label = Printf.sprintf "%s%s %s" arrow check day in
        if is_cur && is_sel
        then s label |> styleBold ++ colorBrightGreen
        else if is_cur
        then s label |> styleBold ++ colorBrightCyan
        else if is_sel
        then s label |> colorBrightGreen
        else s label)
  in
  let sel_count = List.length st.selected in
  let base_data =
    [
      [ 10.0; 45.0; 80.0; 75.0; 50.0; 15.0 ];
      [ 12.0; 50.0; 85.0; 70.0; 55.0; 20.0 ];
      [ 8.0; 40.0; 90.0; 80.0; 60.0; 25.0 ];
      [ 15.0; 55.0; 75.0; 65.0; 45.0; 18.0 ];
      [ 10.0; 48.0; 70.0; 60.0; 35.0; 30.0 ];
      [ 5.0; 15.0; 25.0; 30.0; 40.0; 55.0 ];
      [ 3.0; 10.0; 20.0; 25.0; 35.0; 45.0 ];
    ]
  in
  let heat_data =
    if st.selected = []
    then base_data
    else
      List.mapi
        (fun idx row ->
          if List.mem idx st.selected
          then row
          else List.map (fun v -> v *. 0.15) row)
        base_data
  in
  columns
    [
      box ~title:"Schedule"
        [
          layout selector_lines;
          br;
          (s (Printf.sprintf "%d of %d active" sel_count (Array.length days))
          |> if sel_count > 0 then colorBrightGreen else colorBrightBlack);
        ]
      |> borderRound
      |> colorBrightCyan;
      box ~title:"Weekly Activity"
        [
          plotHeatmap ~cell_width:5
            (heatmap_data ~grid:heat_data ~row_labels:(Array.to_list days)
               ~col_labels:hours);
        ]
      |> borderRound;
    ]

(* ========================================================================
   View
   ======================================================================== *)

let view st =
  let header = render_header st in
  let content =
    match st.scene with
    | 0 -> scene_physics_game st
    | 1 -> scene_text_input st
    | 2 -> scene_borders_styles st
    | 3 -> scene_tables st
    | 4 -> scene_charts_plots st
    | 5 -> scene_bar_charts_sparklines st
    | 6 -> scene_selections_heatmap st
    | _ -> s "Unknown scene"
  in
  let footer = render_footer st in
  left_align ~width:scene_width (layout [ header; br; content; br; footer ])

(* ========================================================================
   App
   ======================================================================== *)

let showcase_app =
  {
    init = (initial_state, CmdNone);
    update = (fun msg st -> update msg st);
    subscriptions = (fun st -> subscriptions st);
    view = (fun st -> view st);
  }

let () =
  run_app ~options:{ default_options with alignment = AlignCenter } showcase_app
