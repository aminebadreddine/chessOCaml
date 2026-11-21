(** A complete chess position.

    Representation: a record. Immutable; every update builds a new record. *)

type t = {
  board : Board.t;
  turn : Color.t;
  castling : Castling.t;
  en_passant : Square.t option;
  halfmove_clock : int;
  fullmove_number : int;
}

let make ~board ~turn ~castling ~en_passant ~halfmove_clock ~fullmove_number =
  { board; turn; castling; en_passant; halfmove_clock; fullmove_number }

let initial =
  {
    board = Board.initial;
    turn = Color.White;
    castling = Castling.all;
    en_passant = None;
    halfmove_clock = 0;
    fullmove_number = 1;
  }

let board pos = pos.board
let turn pos = pos.turn
let castling pos = pos.castling
let en_passant pos = pos.en_passant
let halfmove_clock pos = pos.halfmove_clock
let fullmove_number pos = pos.fullmove_number
let piece_at pos sq = Board.get pos.board sq

let same_position p1 p2 =
  Board.equal p1.board p2.board
  && Color.equal p1.turn p2.turn
  && Castling.equal p1.castling p2.castling
  && p1.en_passant = p2.en_passant

let equal p1 p2 =
  same_position p1 p2
  && p1.halfmove_clock = p2.halfmove_clock
  && p1.fullmove_number = p2.fullmove_number

(* ------------------------------------------------------------------ *)
(* Applying a move                                                      *)
(*                                                                      *)
(* [apply_unchecked] below is complete and correct for ordinary moves:  *)
(* pushes, piece moves, and captures. The five helpers above it each    *)
(* handle one special rule, and each currently does nothing. That is    *)
(* deliberate: V1 is a legal-movement game with no special rules, and   *)
(* each later feature is one helper, implemented and tested on its own, *)
(* with no change to anything that calls them.                          *)
(*                                                                      *)
(* Implement them in this order. Each has a matching test in            *)
(* test/test_chessOCaml.ml, currently in the staged-feature group.      *)
(* ------------------------------------------------------------------ *)

(** [capture_en_passant_victim board pos piece move] is [board] with the pawn
    captured en passant removed.

    TODO (feature: en passant). The move is an en passant capture when [piece]
    is a pawn, [move.to_] is exactly [pos.en_passant], and the pawn changes
    file. The captured pawn is not on [move.to_]: it sits on the square with
    [move.to_]'s file and [move.from]'s rank. In every other case return
    [board] unchanged.

    Hint: [Square.make ~file:(Square.file move.to_) ~rank:(Square.rank
    move.from)] names that square, and [Board.set] with [None] clears it. *)
let capture_en_passant_victim (board : Board.t) (_pos : t) (_piece : Piece.t)
    (_move : Move.t) : Board.t =
  board

(** [promote board piece move] is [board] with the piece on [move.to_] replaced
    by the promotion piece.

    TODO (feature: promotion). Applies when [move.promotion] is [Some kind]. The
    new piece keeps the mover's color. Call this {b after} the pawn has been
    moved onto [move.to_], which is the order [apply_unchecked] uses. In every
    other case return [board] unchanged. *)
let promote (board : Board.t) (_piece : Piece.t) (_move : Move.t) : Board.t =
  board

(** [move_castling_rook board piece move] is [board] with the rook moved to the
    far side of the king.

    TODO (feature: castling). The move is a castle when [piece] is a king and
    the move spans two files. Kingside, the rook goes from the h-file to the
    f-file; queenside, from the a-file to the d-file, on the king's rank. The
    king itself has already been moved by the time this is called. In every
    other case return [board] unchanged.

    Note that this function is only ever handed a legal move. Whether the
    squares between are empty and unattacked is {!Rules}' problem, not
    this one. *)
let move_castling_rook (board : Board.t) (_piece : Piece.t) (_move : Move.t) :
    Board.t =
  board

(** [next_castling_rights rights piece move] is [rights] minus everything the
    move destroys.

    TODO (feature: castling). Three ways a right dies, and all three must be
    handled or castling will be legal after the rook is long gone:

    - the king moves at all, including castling: both of that color's rights go
      ([Castling.remove_color]);
    - a rook moves off a1, h1, a8, or h8: that one right goes;
    - a rook is {b captured} on one of those squares: that right goes, and the
      relevant color is the opponent of the mover. This is the case people
      forget, because nothing about the moving piece signals it; look at
      [move.to_].

    Removing a right that is already gone is harmless, so these cases may be
    written independently. *)
let next_castling_rights (rights : Castling.t) (_piece : Piece.t)
    (_move : Move.t) : Castling.t =
  rights

(** [next_en_passant_target piece move] is the square an enemy pawn could
    capture onto next move, or [None].

    TODO (feature: en passant). Set it only when [piece] is a pawn that moved
    two ranks; the target is the square it skipped over, that is, the one
    between [move.from] and [move.to_]. Every other move clears it, which is
    why returning [None] by default is correct rather than merely a stub. *)
let next_en_passant_target (_piece : Piece.t) (_move : Move.t) :
    Square.t option =
  None

(* The fifty-move rule counts plies since the last irreversible event. *)
let next_halfmove_clock clock (piece : Piece.t) (captured : Piece.t option) =
  if piece.Piece.kind = Piece.Pawn || captured <> None then 0 else clock + 1

let apply_unchecked pos move =
  let from = move.Move.from and to_ = move.Move.to_ in
  let piece =
    match Board.get pos.board from with
    | Some p -> p
    | None ->
        invalid_arg
          ("Position.apply_unchecked: no piece on " ^ Square.to_string from)
  in
  let captured = Board.get pos.board to_ in
  let board = capture_en_passant_victim pos.board pos piece move in
  let board = Board.move_piece board ~from ~to_ in
  let board = promote board piece move in
  let board = move_castling_rook board piece move in
  {
    board;
    turn = Color.opposite pos.turn;
    castling = next_castling_rights pos.castling piece move;
    en_passant = next_en_passant_target piece move;
    halfmove_clock = next_halfmove_clock pos.halfmove_clock piece captured;
    fullmove_number =
      (match pos.turn with
      | Color.White -> pos.fullmove_number
      | Color.Black -> pos.fullmove_number + 1);
  }
