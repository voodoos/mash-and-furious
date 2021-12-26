type state = Released | Pressed

type update = Changed of state | Same of state

type t = state ref

let make ?(init = Released) () : t = ref init

let update ~new_state t =
  if new_state = !t then Same new_state
  else (
    t := new_state;
    Changed new_state)

let pp (fmt : Format.formatter) t =
  Format.fprintf fmt "%s"
    (match !t with Released -> "Released" | _ -> "Pressed ")
