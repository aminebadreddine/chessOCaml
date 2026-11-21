(** The two players. *)

type t =
  | White
  | Black

val opposite : t -> t
(** [opposite c] is the player who is not [c]. *)

val equal : t -> t -> bool
(** [equal c1 c2] is whether [c1] and [c2] are the same player. *)

val to_string : t -> string
(** [to_string c] is ["white"] or ["black"]. *)
