(** Forsyth–Edwards Notation: a position as one line of text.

    FEN is worth having early, for three reasons that have nothing to do with
    being a chess standard:

    - it is how a test says "set up this position" in one line, instead of
      twenty calls to [Board.set];
    - it is how a bug is reported between teammates;
    - it is most of the save format, so persistence costs almost nothing once
      this exists.

    A FEN string has six space-separated fields: piece placement from rank 8
    down to rank 1 with ['/'] between ranks and digits for runs of empty
    squares, the side to move, castling rights, the en passant target, the
    halfmove clock, and the fullmove number. *)

val initial : string
(** [initial] is the FEN of the starting position. *)

val of_position : Position.t -> string
(** [of_position pos] is the FEN of [pos]. *)

val to_position : string -> (Position.t, string) result
(** [to_position s] is [Ok pos] when [s] is a well-formed FEN string, and
    [Error msg] otherwise, where [msg] says what was wrong. It does not check
    that the position is reachable or even sane; a board with nine white kings
    parses. *)

val of_board : Board.t -> string
(** [of_board b] is just the piece-placement field, the first of the six. *)
