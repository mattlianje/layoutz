(* Layoutz OCaml Examples

   Run: dune utop lib
   Then paste these examples!
*)

open Layoutz

(* Use `s` for quick strings, or `text` - they're the same *)

(* === Basic text === *)
let _ = print (s "Hello, Layoutz!")

(* === Horizontal rules === *)
let _ = print hr
let _ = print (hr' ~char:"=" ~width:30 ())

(* === Vertical stacking === *)
let _ = print (layout [ s "Line 1"; s "Line 2"; s "Line 3" ])

(* === Horizontal stacking === *)
let _ = print (row [ s "Left"; s "Middle"; s "Right" ])
let _ = print (row ~tight:true [ s "["; s "no"; s "gaps"; s "]" ])

(* === Boxes === *)
let _ = print (box ~title:"Hello" [ s "World" ])

let _ =
  print (box ~title:"Status" [ s "CPU: 42%"; s "MEM: 8GB"; s "Uptime: 3 days" ])

(* Different border styles *)
let _ = print (box ~title:"Double" [ s "Fancy!" ] |> borderDouble)
let _ = print (box ~title:"Round" [ s "Smooth corners" ] |> borderRound)
let _ = print (box ~title:"Thick" [ s "Bold borders" ] |> borderThick)

(* === Tables === *)
let _ =
  print
    (table
       ~headers:[ s "Name"; s "Age"; s "City" ]
       [
         [ s "Alice"; s "30"; s "NYC" ];
         [ s "Bob"; s "25"; s "LA" ];
         [ s "Carol"; s "35"; s "Chicago" ];
       ])

let _ =
  print
    (table
       ~headers:[ s "Language"; s "Typing"; s "Paradigm" ]
       [
         [ s "OCaml"; s "Static"; s "Functional" ];
         [ s "Python"; s "Dynamic"; s "Multi" ];
         [ s "Rust"; s "Static"; s "Systems" ];
       ]
    |> borderDouble)

(* === Colors and styles === *)
let _ = print (s "Error!" |> fg colorRed)
let _ = print (s "Success" |> fg colorGreen |> styleBold)
let _ = print (s "Warning" |> fg colorYellow |> bg colorBlack)

(* Compose styles with ++ *)
let fancy = styleBold ++ styleItalic ++ styleUnderline
let _ = print (s "Fancy text" |> fancy)

(* === Lists === *)
let _ =
  print
    (ul
       [
         li (s "First item");
         li (s "Second item");
         li ~c:[ li (s "Nested A"); li (s "Nested B") ] (s "With children");
       ])

let _ = print (ol [ li (s "Step one"); li (s "Step two"); li (s "Step three") ])

(* === Trees === *)
let _ =
  print
    (tree
       (node
          ~c:
            [
              node (s "src");
              node ~c:[ node (s "test_main.ml") ] (s "test");
              node (s "README.md");
            ]
          (s "project")))

(* === Key-value pairs === *)
let _ = print (kv [ ("Name", "Alice"); ("Age", "30"); ("Location", "NYC") ])

(* === Progress bars === *)
let _ = print (inline_bar ~label:"Loading" ~progress:0.65)

(* === Charts === *)
let _ = print (chart [ ("OCaml", 85.0); ("Haskell", 72.0); ("Scala", 90.0) ])

(* === Sections === *)
let _ =
  print
    (section ~title:"Status" [ s "All systems operational"; s "Uptime: 99.9%" ])

(* === Alignment === *)
(* Auto-center in layout - width computed from siblings *)
let _ =
  print
    (layout
       [ s "A long title line here"; center (s "Auto centered!"); s "Footer" ])

(* Or explicit width *)
let _ = print (center ~width:30 (s "Explicit width"))
let _ = print (right_align ~width:30 (s "Right"))

(* === Nested layouts === *)
let _ =
  print
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
      box ~title:" System Monitor "
        [
          row
            [
              box ~title:"CPU" [ s "█████░░░░░ 50%" ];
              box ~title:"RAM" [ s "███████░░░ 70%" ];
              box ~title:"Disk" [ s "██░░░░░░░░ 20%" ];
            ];
        ]
      |> borderDouble;
      br;
      table
        ~headers:[ s "Process"; s "PID"; s "CPU%"; s "MEM%" ]
        [
          [ s "chrome"; s "1234"; s "25.0"; s "512M" ];
          [ s "code"; s "5678"; s "15.0"; s "1.2G" ];
          [ s "dune"; s "9012"; s "5.0"; s "128M" ];
        ];
    ]

let _ = print dashboard

(* === README example === *)
let status_card label value = box ~title:label [ s value ]

let readme_example =
  layout
    [
      s "System Dashboard" |> styleBold |> center;
      hr;
      row
        [
          status_card "API" "LIVE" |> fg colorGreen;
          status_card "DB" "99.9%";
          status_card "Cache" "READY" |> fg colorCyan;
        ];
      box ~title:"Services"
        [
          ul [ li (s "Production"); li (s "Staging"); li (s "Development") ];
          inline_bar ~label:"Health" ~progress:0.94;
        ]
      |> borderRound;
    ]

let _ = print readme_example
