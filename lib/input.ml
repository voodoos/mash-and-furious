module type S = sig
  type t

  val read : t -> Two_states.update

  val pp : Format.formatter -> t -> unit
end
