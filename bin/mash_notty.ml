open Mash_attacks
module Mtime = Rpi.Mtime

let num_led = 144

let stop = ref false

let () = Sys.(set_signal sigint (Signal_handle (fun _ -> stop := true)))

let world_size = 41

let middle = world_size / 2

let position = ref (float middle)

let to_0_5 x =
  let x = float x in
  Float.(round (5. *. x /. 255.) |> to_int)

let draw colors =
  let open Notty in
  let circle = "\xe2\x97\x8f" in
  let v = I.char A.(bg lightblack) ' ' 1 1 in
  I.(
    List.fold_left
      (fun a (c : Utils.color) ->
        let color = A.rgb ~r:(to_0_5 c.r) ~g:(to_0_5 c.g) ~b:(to_0_5 c.b) in
        let i = I.(string A.(fg color ++ bg lightblack) circle) in
        I.(a <|> v <|> i))
      I.empty colors
    <|> v)

let games : (module Game.S) list = [ (module Mash_and_furious.Game) ]

let () =
  let open! Notty in
  let open Notty_unix in
  let term = Term.create ~dispose:true ~nosig:false () in

  let counter = Mtime.counter () in

  let module Game = (val List.hd games) (Ma_notty.Key) in
  let state =
    Game.init
      (Game.Player.make (Ma_notty.Key.make { term; key = 'a' }))
      (Game.Player.make (Ma_notty.Key.make { term; key = 'p' }))
  in

  let last_time = ref 0L in

  let rec loop state =
    (* 60 FPS *)
    let offset = Mtime.counter_value_us counter in
    Mtime.sleep_us (Int64.sub 8_000L (Int64.sub offset !last_time));

    if !stop then ()
    else
      let offset = Mtime.counter_value_us counter in
      let state = Game.update ~offset ~dt:offset state in
      let colors = Game.render state in

      let i_state = I.strf "%a" Game.pp state in
      let i_leds = draw colors in
      let i_time = I.strf "%Ld" (Int64.sub offset !last_time) in
      let i = I.(i_state <-> i_leds <-> i_time) in
      Term.image term i;

      last_time := offset;
      loop state
  in
  loop state;
  Term.release term
