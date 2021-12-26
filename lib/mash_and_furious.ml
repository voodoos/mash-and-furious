module Game (Input : Input.S) = struct
  module Player = Player.Make (Input)

  type state = { position : float; p1 : Player.t; p2 : Player.t }

  let num_leds = 144

  let world_size = 41

  let middle = world_size / 2

  let id = "maf"

  let init p1 p2 = { position = float middle; p1; p2 }

  let update ~offset ~dt:_ state =
    let open Player in
    (* Update player bps *)
    let p1, p2 =
      match Player.compute_bpm offset [ state.p1; state.p2 ] with
      | [ p1; p2 ] -> (p1, p2)
      | _ -> assert false
    in

    let position = state.position in
    let rubber =
      if position > float middle then -0.05
      else if position < float middle then 0.05
      else 0.
    in
    let diff = (p1.bps -. p2.bps) /. 50. in
    let diff = if Float.abs diff < 0.01 then rubber else diff in
    let new_position = position +. diff in
    let new_position =
      if new_position < 0. then 0.
      else if new_position > float (world_size - 1) then float (world_size - 1)
      else new_position
    in
    { position = new_position; p1; p2 }

  let render state =
    List.init num_leds (fun i ->
        if i = Float.(round state.position |> to_int) then
          let bps = Float.max state.p1.bps state.p2.bps in
          let d = bps /. 40. in
          if i = 0 || i = world_size - 1 then Utils.{ r = 0; b = 0; g = 60 }
          else { r = 60; b = Float.round (30. *. d) |> Float.to_int; g = 0 }
        else { r = 0; b = 0; g = 0 })

  let pp (fmt : Format.formatter) state =
    Format.fprintf fmt "%a@.%a" Player.pp state.p1 Player.pp state.p2
end

module type S = module type of Game
