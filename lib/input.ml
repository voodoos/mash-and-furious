module Two_states = Utils.Two_states

module type S = sig
  type t

  type config

  val make : config -> t

  val read : t list -> Two_states.update list

  val pp : Format.formatter -> t -> unit
end
