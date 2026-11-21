(** How a game stands. Produced by {!Rules}; consumed by the interface, the
    bot, and the save format. It lives in its own module, below both, so that
    nothing has to depend on the rules just to name an outcome. *)

type draw_reason =
  | Stalemate
  | Fifty_move
  | Threefold_repetition
  | Insufficient_material
  | Agreement

type t =
  | Ongoing
  | Check of Color.t  (** the player who is in check, and still to move *)
  | Checkmate of Color.t  (** the player who has been mated and so loses *)
  | Draw of draw_reason

val is_over : t -> bool
(** [is_over s] is whether no further move may be played. [Check] is not
    over. *)

val to_string : t -> string
(** [to_string s] is a short human-readable description. *)
