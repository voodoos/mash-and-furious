open Rpi
module Two_states = Utils.Two_states

type t = { pin : Gpio.pin; state : Two_states.t }

type config = Gpio.pin

let make pin =
  Gpio.set_func pin F_IN;
  Gpio.set_pull_state pin PULL_DOWN;
  { pin; state = Two_states.make () }

let read ts =
  let open Two_states in
  List.map
    (fun t ->
      let new_state = if Gpio.get t.pin then Pressed else Released in
      update ~new_state t.state)
    ts

let pp (fmt : Format.formatter) t =
  Format.fprintf fmt "%a" Two_states.pp t.state
