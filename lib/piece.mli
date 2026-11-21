(** Chess pieces: a kind together with the player who owns it. *)

type kind =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type t = {
  color : Color.t;
  kind : kind;
}

val make : Color.t -> kind -> t
(** [make color kind] is the piece of that [kind] belonging to [color]. *)

val equal : t -> t -> bool
(** [equal p1 p2] is whether [p1] and [p2] have the same color and kind. *)

val to_char : t -> char
(** [to_char p] is the FEN letter for [p]: uppercase for White, lowercase for
    Black. A white knight is ['N']; a black pawn is ['p']. *)

val of_char : char -> t option
(** [of_char c] is [Some p] when [c] is a FEN piece letter, and [None]
    otherwise. Inverse of {!to_char}. *)

val kind_to_char : kind -> char
(** [kind_to_char k] is the uppercase letter naming [k], ignoring color. *)

val kind_of_char : char -> kind option
(** [kind_of_char c] is [Some k] when [c] names a kind, case-insensitively. *)

val to_string : t -> string
(** [to_string p] is a human-readable name such as ["white knight"]. *)
