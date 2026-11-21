(** The placement of pieces on the 64 squares.

    A board knows {b only} where the pieces are. It does not know whose turn it
    is, what the castling rights are, or which moves are legal; that is
    {!Position}'s job. It does not know how pieces move; that is {!Movegen}'s
    job. Keeping the board ignorant of the rules is what lets move generation
    depend on it without a cycle.

    Boards are immutable. Every operation returns a new board and leaves its
    argument untouched. *)

type t

val empty : t
(** [empty] has no pieces on it. *)

val initial : t
(** [initial] is the standard starting array. *)

val get : t -> Square.t -> Piece.t option
(** [get b sq] is the piece on [sq], or [None] if [sq] is empty. *)

val set : t -> Square.t -> Piece.t option -> t
(** [set b sq p] is [b] with [sq] holding [p], replacing whatever was there. *)

val move_piece : t -> from:Square.t -> to_:Square.t -> t
(** [move_piece b ~from ~to_] is [b] with the contents of [from] moved onto
    [to_], overwriting any piece on [to_], and [from] left empty. It performs no
    legality check and no special-move handling; it is raw placement. If [from]
    is empty, [to_] becomes empty too. *)

val is_empty : t -> Square.t -> bool
(** [is_empty b sq] is whether [sq] holds no piece. *)

val occupied_by : t -> Square.t -> Color.t -> bool
(** [occupied_by b sq color] is whether [sq] holds a piece belonging to
    [color]. *)

val fold : (Square.t -> Piece.t option -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f b init] folds [f] over all 64 squares in index order, empty squares
    included. *)

val to_list : t -> (Square.t * Piece.t) list
(** [to_list b] is every occupied square paired with its piece, in index
    order. *)

val pieces_of : t -> Color.t -> (Square.t * Piece.t) list
(** [pieces_of b color] is every square occupied by [color], in index order.
    Move generation starts here. *)

val find_king : t -> Color.t -> Square.t option
(** [find_king b color] is the square of [color]'s king, or [None] if there is
    none. Check detection starts here. A legal position always has one, but the
    type admits positions built by hand or read from a file, so this cannot
    promise one. *)

val equal : t -> t -> bool
(** [equal b1 b2] is whether the two boards hold the same pieces on the same
    squares. *)

val to_ascii : t -> string
(** [to_ascii b] is a multi-line diagram of [b] with rank 8 at the top, using
    FEN letters and ['.'] for empty squares. This exists so that the engine can
    be debugged before there is a user interface; player-facing rendering is
    {!Render}'s job. *)
