(* Layoutz REPL Examples

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

(* === Layouts (vertical stacking) === *)
let _ = put_str_ln (layout [
  s "Line 1";
  s "Line 2";
  s "Line 3"
])

(* === Rows (horizontal) === *)
let _ = put_str_ln (row [s "Left"; s "Middle"; s "Right"])
let _ = put_str_ln (row ~tight:true [s "["; s "no"; s "gaps"; s "]"])

(* === Boxes === *)
let _ = put_str_ln (box ~title:"Hello" [s "World"])

let _ = put_str_ln (box ~title:"Status" [
  s "CPU: 42%";
  s "MEM: 8GB";
  s "Uptime: 3 days"
])

(* Different border styles *)
let _ = put_str_ln (box ~border:BorderDouble ~title:"Double" [s "Fancy!"])
let _ = put_str_ln (box ~border:BorderRound ~title:"Round" [s "Smooth corners"])
let _ = put_str_ln (box ~border:BorderThick ~title:"Thick" [s "Bold borders"])

(* === Tables === *)
let _ = put_str_ln (table ~headers:["Name"; "Age"; "City"] [
  ["Alice"; "30"; "NYC"];
  ["Bob"; "25"; "LA"];
  ["Carol"; "35"; "Chicago"]
])

let _ = put_str_ln (table ~border:BorderDouble ~headers:["Language"; "Typing"; "Paradigm"] [
  ["OCaml"; "Static"; "Functional"];
  ["Python"; "Dynamic"; "Multi"];
  ["Rust"; "Static"; "Systems"]
])

(* === Nested layouts === *)
let _ = put_str_ln (layout [
  s "=== Dashboard ===";
  br;
  row [
    box ~title:"Users" [s "Online: 42"; s "Total: 1337"];
    box ~title:"Server" [s "Status: OK"; s "Load: 0.5"]
  ];
  br;
  table ~headers:["Event"; "Time"; "Status"] [
    ["Deploy"; "10:30"; "Success"];
    ["Backup"; "11:00"; "Running"];
    ["Sync"; "11:30"; "Pending"]
  ]
])

(* === Quick dashboard === *)
let dashboard = layout [
  box ~border:BorderDouble ~title:" System Monitor " [
    row [
      box ~title:"CPU" [s "█████░░░░░ 50%"];
      box ~title:"RAM" [s "███████░░░ 70%"];
      box ~title:"Disk" [s "██░░░░░░░░ 20%"]
    ]
  ];
  br;
  table ~headers:["Process"; "PID"; "CPU%"; "MEM%"] [
    ["chrome"; "1234"; "25.0"; "512M"];
    ["code"; "5678"; "15.0"; "1.2G"];
    ["dune"; "9012"; "5.0"; "128M"]
  ]
]

let _ = put_str_ln dashboard
