module Two_states = Utils.Two_states

let next_id =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    !counter

module Make (Input : Input.S) = struct
  type t = { id : int; input : Input.t; last_press : int64; bps : float }

  let make input = { id = next_id (); input; last_press = 0L; bps = 0. }

  let us_in_a_s = Int64.to_float Rpi.Mtime.s_to_us

  let compute_bpm time ts =
    List.map2
      (fun update t ->
        match update with
        | Two_states.Changed Pressed ->
            let elapsed_time = Int64.sub time t.last_press in
            (* TODO SMOOTH (moyenne glissante) *)
            let bps = us_in_a_s /. Int64.to_float elapsed_time in
            { t with last_press = time; bps }
        | _ -> { t with bps = t.bps -. (5. /. 100. *. t.bps) })
      (Input.read (List.map (fun t -> t.input) ts))
      ts

  let pp (fmt : Format.formatter) t =
    Format.fprintf fmt "Player %i: %a: %f" t.id Input.pp t.input (60. *. t.bps)
end

module type S = module type of Make
