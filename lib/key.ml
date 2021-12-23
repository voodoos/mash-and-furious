open Notty_unix

type t = { term : Term.t; key : Notty.Unescape.key; state : Two_states.t }

let make term key = { term; key = (`ASCII key, []); state = Two_states.make () }

let read t =
  let new_state =
    if Term.pending t.term then
      match Term.event t.term with
      | `Key k when k = t.key -> Two_states.Pressed
      | _ -> Two_states.Released
    else Two_states.Released
  in
  Two_states.update ~new_state t.state

let pp (fmt : Format.formatter) t =
  Format.fprintf fmt "%a" Two_states.pp t.state
