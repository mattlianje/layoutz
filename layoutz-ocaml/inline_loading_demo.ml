open Layoutz

(* Inline loading demo — progress bars that render in-place without
   clearing the screen. Prior output stays visible.

   Run with: dune exec ./inline_loading_demo.exe *)

type load_state = { progress: float; done_ticks: int }
type load_msg = Tick

let inline_options =
  { default_options with clear_on_start = false; clear_on_exit = false }

let progress_bar label speed =
  {
    init = ({ progress = 0.0; done_ticks = 0 }, CmdNone);
    update =
      (fun Tick st ->
        if st.done_ticks > 20
        then (st, CmdExit)
        else if st.progress >= 1.0
        then ({ st with done_ticks = st.done_ticks + 1 }, CmdNone)
        else ({ st with progress = min 1.0 (st.progress +. speed) }, CmdNone));
    subscriptions = (fun _ -> sub_every_ms 16 Tick);
    view =
      (fun st ->
        let w = 40 in
        let filled = int_of_float (st.progress *. float_of_int w) in
        let bar =
          List.init w (fun i ->
              if i < filled
              then
                let ratio = float_of_int i /. float_of_int w in
                let r = int_of_float (ratio *. 180.0) + 50 in
                let g = int_of_float ((1.0 -. ratio) *. 200.0) + 55 in
                s "█" |> colorRGB r g 255
              else s "░" |> colorBrightBlack)
        in
        let pct = Printf.sprintf "%d%%" (int_of_float (st.progress *. 100.0)) in
        layout [ tightRow bar; s (label ^ " " ^ pct) |> colorBrightCyan ]);
  }

let sleep_ms ms = Unix.sleepf (float_of_int ms /. 1000.0)

let () =
  print_endline "hello from a normal process";
  sleep_ms 800;
  print_endline "doing some work...";
  sleep_ms 700;
  print_endline "now watch this:";
  sleep_ms 800;
  print_newline ();
  run_app ~options:inline_options (progress_bar "Fetching deps..." 0.018);
  run_app ~options:inline_options (progress_bar "Building..." 0.010);
  run_app ~options:inline_options (progress_bar "Linking..." 0.025);
  print_newline ();
  print_endline "back to normal output"
