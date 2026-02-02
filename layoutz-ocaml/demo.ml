open Layoutz

(* Define layouts *)
let t =
  table
    ~headers:[ s "Name"; s "Role"; s "Status" ]
    [
      [ s "Alice"; s "Engineer"; s "Online" ];
      [ s "Eve"; s "QA"; s "Away" ];
      [
        ul
          [
            li
              ~c:[ li ~c:[ li (s "was a BAD man") ] (s "Mousasi") ]
              (s "Gegard");
          ];
        s "Fighter";
        s "Nasty";
      ];
    ]
  |> borderRound

(* Color gradient *)
let gradient =
  let indices = List.init 22 (fun i -> i * 12) in
  List.map
    (fun i ->
      let r = if i < 128 then i * 2 else 255 in
      let g = if i < 128 then 255 else (255 - i) * 2 in
      let b = if i > 128 then (i - 128) * 2 else 0 in
      s "█" |> fg (colorRGB r g b))
    indices

(* Nest, compose, combine them *)
let d =
  layout
    [
      center
        (row
           [
             underlineColored ~char:"^" ~color:colorBrightMagenta
               (s "Layoutz" |> styleBold);
             s "... A Small Demo (ちいさい)";
           ]);
      row
        [
          statusCard ~label:(s "Users") ~content:(s "1.2K")
          |> fg colorBrightBlue;
          statusCard ~label:(s "API") ~content:(s "UP")
          |> borderDouble
          |> fg colorBrightGreen;
          statusCard ~label:(s "CPU") ~content:(s "23%")
          |> borderThick
          |> fg colorBrightYellow;
          t;
          section ~title:"Pugilists"
            [
              kv
                [
                  ("Kazushi", "Sakuraba");
                  ("Jet 李連杰", "Li");
                  ("Rory", "MacDonald");
                ];
              tightRow gradient;
            ];
        ];
      row
        [
          layout
            [
              box ~title:"Wrapped"
                [
                  wrap ~max_width:20
                    (s "Where there is a will ... Water x Necessaries");
                ]
              |> fg colorBrightMagenta
              |> styleReverse ++ styleBold;
              ol
                [
                  li (s "Arcole");
                  li (s "Austerlitz");
                  li ~c:[ li ~c:[ li (s "Бородино") ] (s "Iéna") ] (s "Wagram");
                ];
            ];
          layout
            [
              margin ~prefix:"[OCaml!]"
                (layout
                   [
                     box ~title:"Deploy Status"
                       [
                         inline_bar ~label:"Build" ~progress:1.0;
                         inline_bar ~label:"Test" ~progress:0.8;
                         inline_bar ~label:"Deploy" ~progress:0.3;
                       ]
                     |> fg colorGreen;
                   ]);
              tree
                (node
                   ~c:
                     [
                       node
                         ~c:[ node (s "main.ml"); node (s "test.ml") ]
                         (s "src");
                     ]
                   (s "📁 Project"))
              |> fg colorCyan;
            ];
        ];
    ]

(* Get pretty strings w/ render, or just print *)
let () = print d
