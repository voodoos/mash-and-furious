open Mash_attacks
open Rpi
module Player = Player.ButtonPlayer

let num_led = 144

let reset_frame =
  List.init 144 (fun _ -> { Ws2812b.r = 0; g = 0; b = 0 }) |> Ws2812b.encode

let cut = 16

let led_off = { Ws2812b.r = 0; b = 0; g = 0 }

let pattern =
  List.init num_led (fun i ->
      if i < 3 then { Ws2812b.r = 60; b = 0; g = 5 } else led_off)

let pattern_off = List.init num_led (fun _ -> led_off)

let stop = ref false

let () = Sys.(set_signal sigint (Signal_handle (fun _ -> stop := true)))

let pat b = if b then pattern else pattern_off

let world_size = 41

let middle = world_size / 2

let position = ref (float middle)

let update _dt p1 p2 =
  let open Player in
  let rubber =
    if !position > float middle then -0.05
    else if !position < float middle then 0.05
    else 0.
  in
  let diff = (p1.bps -. p2.bps) /. 50. in
  let diff = if Float.abs diff < 0.01 then rubber else diff in
  let new_position = !position +. diff in
  let new_position =
    if new_position < 0. then 0.
    else if new_position > float (world_size - 1) then float (world_size - 1)
    else new_position
  in
  position := new_position

let draw () =
  let open Notty in
  let circle = "\xe2\x97\x8f" in
  let v = I.char A.(bg lightblack) ' ' 1 1 in
  I.(
    List.init world_size (fun i ->
        let color =
          if i = Float.(round !position |> to_int) then A.red else A.black
        in
        I.(string A.(fg color ++ bg lightblack) circle))
    |> List.fold_left (fun a b -> I.(a <|> v <|> b)) I.empty
    <|> v)

let draw_leds p1 p2 =
  List.init num_led (fun i ->
      if i = Float.(round !position |> to_int) then
        let bps = Float.max p1.Player.bps p2.Player.bps in
        let d = bps /. 40. in
        if i = 0 || i = world_size - 1 then { Ws2812b.r = 0; b = 0; g = 60 }
        else
          { Ws2812b.r = 60; b = Float.round (30. *. d) |> Float.to_int; g = 0 }
      else led_off)

let () =
  let open! Notty in
  let open Notty_unix in
  let term = Term.create ~dispose:true ~nosig:false () in

  let counter = Mtime.counter () in
  let p1 = Player.make (Button.make Gpio.P17) in
  let p2 = Player.make (Button.make Gpio.P13) in

  (* let p1 = Player.make (Key.make term 'a') in
     let p2 = Player.make (Key.make term 'p') in *)
  let last_time = ref 0L in

  let rec loop p1 p2 =
    (* 60 FPS *)
    let offset = Mtime.counter_value_us counter in
    Mtime.sleep_us (Int64.sub 8_000L (Int64.sub offset !last_time));

    if !stop then Ws2812b.output reset_frame
    else
      let offset = Mtime.counter_value_us counter in
      let p1 = Player.compute_bpm offset p1 in
      let p2 = Player.compute_bpm offset p2 in
      update offset p1 p2;

      (*
      let i_p1 = I.strf "%a" Player.pp p1 in
      let i_p2 = I.strf "%a" Player.pp p2 in
      let i_time = I.strf "%Ld" (Int64.sub offset !last_time) in
      let i_leds = draw () in
      let _i = I.(i_p1 <-> i_p2 <-> i_leds <-> i_time) in
      Term.image term _i; *)
      last_time := offset;
      Ws2812b.output (Ws2812b.encode (draw_leds p1 p2));
      loop p1 p2
  in
  loop p1 p2;
  Term.release term
