open Notty_unix
module Two_states = Utils.Two_states

type t = { term : Term.t; key : Notty.Unescape.key; state : Two_states.t }

type config = { term : Term.t; key : char }

let make { term; key } =
  { term; key = (`ASCII key, []); state = Two_states.make () }

let event ~delay t =
  if Term.pending t then Term.event t
  else
    let open Unix in
    match select [ Term.fds t |> fst ] [] [] delay with
    | [], _, _ -> `End
    | _ :: _, _, _ -> Term.event t
    | exception Unix_error (EINTR, _, _) -> Term.event t

let read (ts : t list) =
  let new_state =
    match event ~delay:0. (List.hd ts).term with
    | `Key k ->
        List.map
          (fun (t : t) ->
            if k = t.key then Two_states.Pressed else Two_states.Released)
          ts
    | _ -> List.map (fun _ -> Two_states.Released) ts
  in
  List.map2
    (fun new_state t -> Two_states.update ~new_state t.state)
    new_state ts

let pp (fmt : Format.formatter) t =
  Format.fprintf fmt "%a" Two_states.pp t.state
