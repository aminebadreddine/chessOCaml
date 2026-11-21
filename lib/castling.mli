(** Castling rights: which of the four castles are still available.

    A right is lost permanently as soon as the king or the relevant rook moves,
    or the rook is captured. Rights say nothing about whether a castle is legal
    {b right now} — that also requires empty squares and an unattacked path,
    which is {!Rules}' business. *)

type side =
  | Kingside
  | Queenside

type t

val none : t
(** [none] has no rights at all. *)

val all : t
(** [all] has all four rights, as at the start of a game. *)

val mem : t -> Color.t -> side -> bool
(** [mem t color side] is whether [color] may still castle to [side]. *)

val add : t -> Color.t -> side -> t
(** [add t color side] is [t] with that right granted. Needed when reading a
    position from FEN; a game never gains rights. *)

val remove : t -> Color.t -> side -> t
(** [remove t color side] is [t] with that right withdrawn. Removing a right
    that is already absent changes nothing. *)

val remove_color : t -> Color.t -> t
(** [remove_color t color] withdraws both of [color]'s rights, as when the king
    moves. *)

val equal : t -> t -> bool

val to_string : t -> string
(** [to_string t] is the FEN field: ["KQkq"], a subset in that order, or ["-"]
    when no rights remain. *)

val of_string : string -> t option
(** [of_string s] parses the FEN field. Inverse of {!to_string}. *)
