open Mash_attacks
open Rpi

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

let to_0_5 x =
  let x = float x in
  Float.(round (5. *. x /. 255.) |> to_int)

let games : (module Game.S) list = [ (module Mash_and_furious.Game) ]

let () =
  let counter = Mtime.counter () in

  let module Game = (val List.hd games) (Ma_garland.Button) in
  let state =
    Game.init
      (Game.Player.make (Ma_garland.Button.make Gpio.P17))
      (Game.Player.make (Ma_garland.Button.make Gpio.P13))
  in

  let last_time = ref 0L in

  let rec loop state =
    (* 60 FPS *)
    let offset = Mtime.counter_value_us counter in
    Mtime.sleep_us (Int64.sub 8_000L (Int64.sub offset !last_time));

    if !stop then Ws2812b.output reset_frame
    else
      let offset = Mtime.counter_value_us counter in
      let state = Game.update ~offset ~dt:offset state in
      let colors = Game.render state in

      Ws2812b.output (Ws2812b.encode colors);

      last_time := offset;
      loop state
  in
  loop state
