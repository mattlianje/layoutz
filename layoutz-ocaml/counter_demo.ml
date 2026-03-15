open Layoutz

(* A simple counter app demonstrating the Elm Architecture runtime *)

type msg = Inc | Dec | Tick

let () =
  run_app
    {
      init = (0, CmdNone);
      update =
        (fun msg count ->
          match msg with
          | Inc -> (count + 1, CmdNone)
          | Dec -> (count - 1, CmdNone)
          | Tick -> (count, CmdNone));
      subscriptions =
        (fun _ ->
          sub_batch
            [
              sub_key_press (fun key ->
                  match key with
                  | KeyUp -> Some Inc
                  | KeyDown -> Some Dec
                  | KeyChar '+' -> Some Inc
                  | KeyChar '-' -> Some Dec
                  | _ -> None);
              sub_every_ms 1000 Tick;
            ]);
      view =
        (fun count ->
          layout
            [
              center (s "Counter Demo" |> styleBold |> colorBrightCyan);
              s "";
              row
                [
                  statusCard ~label:(s "Count")
                    ~content:(s (string_of_int count))
                  |> borderRound
                  |> colorBrightGreen;
                ];
              s "";
              s "  Up/+ to increment, Down/- to decrement" |> colorBrightBlack;
              s "  Ctrl+Q to quit" |> colorBrightBlack;
            ]);
    }
