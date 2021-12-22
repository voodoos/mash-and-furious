let next_id =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    !counter

type t = { id : int; button : Button.t; last_press : int64; bps : float }

let make pin =
  { id = next_id (); button = Button.make pin; last_press = 0L; bps = 0. }

let us_in_a_s = Int64.to_float Rpi.Mtime.s_to_us

let compute_bpm time t =
  match Button.read t.button with
  | Changed Pressed ->
      let elapsed_time = Int64.sub time t.last_press in
      (* TODO SMOOTH (moyenne glissante) *)
      let bps = us_in_a_s /. Int64.to_float elapsed_time in
      { t with last_press = time; bps }
  | _ -> t

let pp (fmt : Format.formatter) t =
  Format.fprintf fmt "Player %i: %a: %f" t.id Button.pp t.button (60. *. t.bps)
