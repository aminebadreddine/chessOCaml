(** A move, as a request to move a piece from one square to another.

    A move is deliberately position-independent: it records only where a piece
    starts, where it lands, and what a pawn promotes to. It does {b not} record
    whether the move captures, castles, or takes en passant.

    That is on purpose. A user typing ["e2e4"], a parser, and a bot can all
    build a move without consulting the board. Whether a move is a capture or a
    castle is a fact about the {b position}, which {!Position.apply_unchecked}
    works out when the move is played. This keeps one move type for the whole
    team instead of a variant that only move generation could construct.

    Castling is encoded as the king's move: White castling kingside is e1 to g1.
    En passant is encoded as the capturing pawn's diagonal move. *)

type t = {
  from : Square.t;
  to_ : Square.t;
  promotion : Piece.kind option;
}

val make : ?promotion:Piece.kind -> Square.t -> Square.t -> t
(** [make ?promotion from to_] is the move from [from] to [to_]. *)

val equal : t -> t -> bool
(** [equal m1 m2] is whether the two moves agree in all three fields. *)

val to_string : t -> string
(** [to_string m] is long algebraic notation: ["e2e4"], or ["e7e8q"] for a
    promotion. This is the format the test suite and save files use. It is not
    standard algebraic notation (["Nf3"]), which belongs in {!Notation}. *)

val of_string : string -> t option
(** [of_string s] is [Some m] when [s] is long algebraic notation of four or
    five characters, and [None] otherwise. Inverse of {!to_string}. *)
