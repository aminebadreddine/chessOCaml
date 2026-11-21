(** The 64 squares of the board.

    A square is abstract. Files run 0..7 for a..h; ranks run 0..7 for 1..8, so
    rank 0 is White's back rank. Nothing outside this module may assume how a
    square is stored. *)

type t

val make : file:int -> rank:int -> t option
(** [make ~file ~rank] is [Some sq] when both coordinates are in 0..7, and
    [None] otherwise. *)

val file : t -> int
(** [file sq] is the file of [sq], 0 for the a-file through 7 for the h-file. *)

val rank : t -> int
(** [rank sq] is the rank of [sq], 0 for rank 1 through 7 for rank 8. *)

val of_index : int -> t option
(** [of_index i] is [Some sq] when [0 <= i < 64]. Indices run a1, b1, ..., h1,
    a2, ... h8. Use this only where a dense index is genuinely needed, such as
    an array-backed table. *)

val to_index : t -> int
(** [to_index sq] is the index of [sq] in 0..63. See {!of_index}. *)

val of_string : string -> t option
(** [of_string s] is [Some sq] when [s] is algebraic coordinates such as ["e4"],
    and [None] otherwise. The file letter may be upper or lower case. *)

val to_string : t -> string
(** [to_string sq] is the algebraic name of [sq], such as ["e4"]. *)

val shift : t -> df:int -> dr:int -> t option
(** [shift sq ~df ~dr] is the square [df] files and [dr] ranks away from [sq],
    or [None] if that would leave the board. This is the workhorse for move
    generation: a white pawn pushes with [~df:0 ~dr:1], and a knight has eight
    offsets. *)

val all : t list
(** [all] is every square, in index order. *)

val equal : t -> t -> bool
val compare : t -> t -> int
