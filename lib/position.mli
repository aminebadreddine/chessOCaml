(** A complete chess position: everything needed to resume a game from this
    point, and nothing else.

    A position is the board plus the four pieces of state that the rules make
    the board alone insufficient for:

    - whose turn it is;
    - the castling rights that survive;
    - the en passant target square, if the last move was a double pawn push;
    - the halfmove clock, for the fifty-move rule, and the fullmove number.

    Those fields exist from the first commit even though the rules that consume
    them land later. Adding them afterwards would mean rewriting every function
    that touches a position.

    A position does {b not} know which moves are legal. {!apply_unchecked} will
    play any move it is handed. Legality lives in {!Rules}, above
    {!Movegen}. *)

type t

val make :
  board:Board.t ->
  turn:Color.t ->
  castling:Castling.t ->
  en_passant:Square.t option ->
  halfmove_clock:int ->
  fullmove_number:int ->
  t
(** [make ~board ~turn ~castling ~en_passant ~halfmove_clock ~fullmove_number]
    is the position with those components. No consistency checking is done:
    callers may build positions that could not arise in a real game, which is
    what test positions and FEN strings need. *)

val initial : t
(** [initial] is the standard starting position, White to move. *)

val board : t -> Board.t
val turn : t -> Color.t
val castling : t -> Castling.t

val en_passant : t -> Square.t option
(** [en_passant pos] is the square a pawn may be captured {b onto}, not the
    square the pawn stands on. After White plays e2-e4 it is e3. It is set
    whenever a pawn double-pushes, whether or not a capture is actually
    available. *)

val halfmove_clock : t -> int
(** [halfmove_clock pos] is the number of plies since the last capture or pawn
    move. At 100 the fifty-move rule applies. *)

val fullmove_number : t -> int
(** [fullmove_number pos] starts at 1 and increases after each Black move. *)

val piece_at : t -> Square.t -> Piece.t option
(** [piece_at pos sq] is [Board.get (board pos) sq]. *)

val apply_unchecked : t -> Move.t -> t
(** [apply_unchecked pos move] is the position after [move] is played in [pos].

    {b It does not check legality.} It assumes [move] came from
    {!Rules.legal_moves} or an equivalent source, and it will cheerfully play a
    bishop in a straight line or leave a king in check. The name says so, and
    the name should not be changed: the whole architecture rests on this
    function being usable by {!Movegen}, which is what legality is defined in
    terms of and so cannot itself depend on it.

    Given a legal move it:

    - moves the piece and removes anything standing on the destination;
    - removes the pawn captured en passant, which is {b not} on the destination
      square;
    - moves the rook when the king castles;
    - replaces a promoting pawn with the promotion piece;
    - withdraws castling rights lost by the move;
    - sets or clears the en passant target;
    - updates both clocks and hands the turn to the other player.

    @raise Invalid_argument if the origin square is empty. *)

val same_position : t -> t -> bool
(** [same_position p1 p2] is whether the two positions are the same for the
    purposes of threefold repetition: identical boards, the same player to
    move, the same castling rights, and the same en passant target. The clocks
    are deliberately ignored, because repetition is about the position, not
    about how the game reached it. *)

val equal : t -> t -> bool
(** [equal p1 p2] is whether every component matches, clocks included. *)
