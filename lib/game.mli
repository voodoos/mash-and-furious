module type S = functor (Input : Input.S) -> sig
  module Player : module type of Player.Make (Input)

  type state

  val id : string

  val init : Player.t -> Player.t -> state

  val update : offset:int64 -> dt:int64 -> state -> state

  val render : state -> Ws2812b.color list

  val pp : Format.formatter -> state -> unit
end
