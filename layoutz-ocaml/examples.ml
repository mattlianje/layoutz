(* Layoutz OCaml Examples

   Run: dune utop lib
   Then paste these examples!
*)

open Layoutz

(* Use `s` for quick strings, or `text` - they're the same *)

(* === Basic text === *)
let _ = put_str_ln (s "Hello, Layoutz!")

(* === Horizontal rules === *)
let _ = put_str_ln hr
let _ = put_str_ln (hr' ~char:"=" ~width:30 ())

(* === Vertical stacking === *)
let _ = put_str_ln (layout [ s "Line 1"; s "Line 2"; s "Line 3" ])

(* === Horizontal stacking === *)
let _ = put_str_ln (row [ s "Left"; s "Middle"; s "Right" ])
let _ = put_str_ln (row ~tight:true [ s "["; s "no"; s "gaps"; s "]" ])

(* === Boxes === *)
let _ = put_str_ln (box ~title:"Hello" [ s "World" ])

let _ =
  put_str_ln
    (box ~title:"Status" [ s "CPU: 42%"; s "MEM: 8GB"; s "Uptime: 3 days" ])

(* Different border styles *)
let _ = put_str_ln (box ~border:borderDouble ~title:"Double" [ s "Fancy!" ])

let _ =
  put_str_ln (box ~border:borderRound ~title:"Round" [ s "Smooth corners" ])

let _ = put_str_ln (box ~border:borderThick ~title:"Thick" [ s "Bold borders" ])

(* === Tables === *)
let _ =
  put_str_ln
    (table
       ~headers:[ s "Name"; s "Age"; s "City" ]
       [
         [ s "Alice"; s "30"; s "NYC" ];
         [ s "Bob"; s "25"; s "LA" ];
         [ s "Carol"; s "35"; s "Chicago" ];
       ])

let _ =
  put_str_ln
    (table ~border:borderDouble
       ~headers:[ s "Language"; s "Typing"; s "Paradigm" ]
       [
         [ s "OCaml"; s "Static"; s "Functional" ];
         [ s "Python"; s "Dynamic"; s "Multi" ];
         [ s "Rust"; s "Static"; s "Systems" ];
       ])

(* === Colors and styles === *)
let _ = put_str_ln (s "Error!" |> fg colorRed)
let _ = put_str_ln (s "Success" |> fg colorGreen |> style styleBold)
let _ = put_str_ln (s "Warning" |> fg colorYellow |> bg colorBlack)

(* Compose styles with ++ *)
let fancy = styleBold ++ styleItalic ++ styleUnderline
let _ = put_str_ln (s "Fancy text" |> style fancy)

(* === Lists === *)
let _ =
  put_str_ln
    (ul
       [
         li (s "First item");
         li (s "Second item");
         lic ~c:[ li (s "Nested A"); li (s "Nested B") ] (s "With children");
       ])

let _ =
  put_str_ln (ol [ li (s "Step one"); li (s "Step two"); li (s "Step three") ])

(* === Trees === *)
let _ =
  put_str_ln
    (tree
       (node
          ~children:
            [
              node (s "src");
              node ~children:[ node (s "test_main.ml") ] (s "test");
              node (s "README.md");
            ]
          (s "project")))

(* === Key-value pairs === *)
let _ =
  put_str_ln (kv [ ("Name", "Alice"); ("Age", "30"); ("Location", "NYC") ])

(* === Progress bars === *)
let _ = put_str_ln (inline_bar ~label:"Loading" ~progress:0.65)

(* === Charts === *)
let _ =
  put_str_ln (chart [ ("OCaml", 85.0); ("Haskell", 72.0); ("Scala", 90.0) ])

(* === Sections === *)
let _ =
  put_str_ln
    (section ~title:"Status" [ s "All systems operational"; s "Uptime: 99.9%" ])

(* === Alignment === *)
(* Auto-center in layout - width computed from siblings *)
let _ =
  put_str_ln
    (layout
       [ s "A long title line here"; center (s "Auto centered!"); s "Footer" ])

(* Or explicit width *)
let _ = put_str_ln (center ~width:30 (s "Explicit width"))
let _ = put_str_ln (right_align ~width:30 (s "Right"))

(* === Nested layouts === *)
let _ =
  put_str_ln
    (layout
       [
         s "=== Dashboard ===";
         br;
         row
           [
             box ~title:"Users" [ s "Online: 42"; s "Total: 1337" ];
             box ~title:"Server" [ s "Status: OK"; s "Load: 0.5" ];
           ];
         br;
         table
           ~headers:[ s "Event"; s "Time"; s "Status" ]
           [
             [ s "Deploy"; s "10:30"; s "Success" |> fg colorGreen ];
             [ s "Backup"; s "11:00"; s "Running" |> fg colorYellow ];
             [ s "Sync"; s "11:30"; s "Pending" |> fg colorCyan ];
           ];
       ])

(* === Full dashboard example === *)
let dashboard =
  layout
    [
      box ~border:borderDouble ~title:" System Monitor "
        [
          row
            [
              box ~title:"CPU" [ s "█████░░░░░ 50%" ];
              box ~title:"RAM" [ s "███████░░░ 70%" ];
              box ~title:"Disk" [ s "██░░░░░░░░ 20%" ];
            ];
        ];
      br;
      table
        ~headers:[ s "Process"; s "PID"; s "CPU%"; s "MEM%" ]
        [
          [ s "chrome"; s "1234"; s "25.0"; s "512M" ];
          [ s "code"; s "5678"; s "15.0"; s "1.2G" ];
          [ s "dune"; s "9012"; s "5.0"; s "128M" ];
        ];
    ]

let _ = put_str_ln dashboard

(* === README example === *)
let status_card label value = box ~title:label [ s value ]

let readme_example =
  layout
    [
      s "System Dashboard" |> style styleBold |> center;
      hr;
      row
        [
          status_card "API" "LIVE" |> fg colorGreen;
          status_card "DB" "99.9%";
          status_card "Cache" "READY" |> fg colorCyan;
        ];
      box ~border:borderRound ~title:"Services"
        [
          ul [ li (s "Production"); li (s "Staging"); li (s "Development") ];
          inline_bar ~label:"Health" ~progress:0.94;
        ];
    ]

let _ = put_str_ln readme_example
