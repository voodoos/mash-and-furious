open Rpi

type state = Released | Pressed

type update = Changed of state | Same of state

type t = { pin : Gpio.pin; mutable state : state }

let make pin =
  Gpio.set_func pin F_IN;
  Gpio.set_pull_state pin PULL_DOWN;
  { pin; state = Released }

let read t =
  let new_state = if Gpio.get t.pin then Pressed else Released in
  if new_state = t.state then Same new_state
  else (
    t.state <- new_state;
    Changed new_state)

let pp (fmt : Format.formatter) t =
  Format.fprintf fmt "%s"
    (match t.state with Released -> "Released" | _ -> "Pressed ")
