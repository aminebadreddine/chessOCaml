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
(* [apply_unchecked] at the bottom handles the parts of a move that are *)
(* the same for every piece: vacate the origin, occupy the destination, *)
(* pass the turn, update the clocks. Each special rule is one helper    *)
(* above it, and each helper returns its input unchanged when the move  *)
(* is not the kind it cares about. That is what keeps [apply_unchecked] *)
(* readable as a list of steps rather than a nest of conditionals, and  *)
(* it is why adding a rule never meant editing the function that calls  *)
(* them.                                                                *)
(* ------------------------------------------------------------------ *)

(* The four corner squares, with the castling right each one carries. A rook
   leaving one of these, or being captured on one, destroys that right. *)
let corner_right sq =
  match (Square.file sq, Square.rank sq) with
  | 0, 0 -> Some (Color.White, Castling.Queenside)
  | 7, 0 -> Some (Color.White, Castling.Kingside)
  | 0, 7 -> Some (Color.Black, Castling.Queenside)
  | 7, 7 -> Some (Color.Black, Castling.Kingside)
  | _ -> None

(** [capture_en_passant_victim board pos piece move] is [board] with the pawn
    captured en passant removed, or [board] unchanged if this is not an en
    passant capture.

    The move is one exactly when a pawn moves onto the en passant target square
    while changing file. The captured pawn is not on that square — it is beside
    the capturing pawn's starting square, which is why this cannot be left to
    the ordinary capture handling in {!apply_unchecked}. *)
let capture_en_passant_victim (board : Board.t) (pos : t) (piece : Piece.t)
    (move : Move.t) : Board.t =
  let from = move.Move.from and to_ = move.Move.to_ in
  let is_en_passant =
    piece.Piece.kind = Piece.Pawn
    && Square.file from <> Square.file to_
    && match pos.en_passant with
       | Some target -> Square.equal target to_
       | None -> false
  in
  if not is_en_passant then board
  else
    (* The victim shares the destination's file and the origin's rank. *)
    match Square.make ~file:(Square.file to_) ~rank:(Square.rank from) with
    | Some victim -> Board.set board victim None
    | None -> board

(** [promote board piece move] is [board] with the piece on the destination
    replaced by the promotion piece, keeping the mover's color.

    Called after the pawn has been moved onto the destination, so this
    overwrites it. *)
let promote (board : Board.t) (piece : Piece.t) (move : Move.t) : Board.t =
  match move.Move.promotion with
  | None -> board
  | Some kind ->
      Board.set board move.Move.to_ (Some (Piece.make piece.Piece.color kind))

(** [move_castling_rook board piece move] moves the rook to the far side of the
    king when [move] is a castle.

    A castle is the only move in which a king changes file by two, so that is
    the test. The king has already been moved by the time this is called;
    whether the squares between were empty and unattacked is {!Rules}'
    problem. *)
let move_castling_rook (board : Board.t) (piece : Piece.t) (move : Move.t) :
    Board.t =
  let from = move.Move.from and to_ = move.Move.to_ in
  let distance = Square.file to_ - Square.file from in
  if piece.Piece.kind <> Piece.King || abs distance <> 2 then board
  else
    let rank = Square.rank from in
    (* Kingside the rook jumps h to f; queenside, a to d. *)
    let rook_from_file, rook_to_file =
      if distance > 0 then (7, 5) else (0, 3)
    in
    match
      ( Square.make ~file:rook_from_file ~rank,
        Square.make ~file:rook_to_file ~rank )
    with
    | Some rook_from, Some rook_to ->
        Board.move_piece board ~from:rook_from ~to_:rook_to
    | _ -> board

(** [next_castling_rights rights piece move] is [rights] minus every right the
    move destroys.

    Three independent ways a right dies, and all three must be handled or
    castling stays legal after the rook is long gone. Removing a right that is
    already absent is harmless, so the three cases compose by simply being
    applied in turn. *)
let next_castling_rights (rights : Castling.t) (piece : Piece.t)
    (move : Move.t) : Castling.t =
  (* 1. The king moves at all, castling included: both of its rights go. *)
  let rights =
    if piece.Piece.kind = Piece.King then
      Castling.remove_color rights piece.Piece.color
    else rights
  in
  (* 2. A rook leaves its corner. *)
  let rights =
    match corner_right move.Move.from with
    | Some (color, side) when piece.Piece.kind = Piece.Rook ->
        Castling.remove rights color side
    | _ -> rights
  in
  (* 3. Something lands on a corner, capturing whatever stood there. Nothing
     about the moving piece signals this; the destination is the only clue,
     and the right lost belongs to the opponent, not the mover. *)
  match corner_right move.Move.to_ with
  | Some (color, side) -> Castling.remove rights color side
  | None -> rights

(** [next_en_passant_target piece move] is the square an enemy pawn could
    capture onto next move, or [None].

    Set only by a pawn moving two ranks; the target is the square it skipped.
    Every other move clears it, which is why [None] is the common case rather
    than an oversight. *)
let next_en_passant_target (piece : Piece.t) (move : Move.t) : Square.t option =
  let from = move.Move.from and to_ = move.Move.to_ in
  if piece.Piece.kind <> Piece.Pawn then None
  else if abs (Square.rank to_ - Square.rank from) <> 2 then None
  else
    Square.make ~file:(Square.file from)
      ~rank:((Square.rank from + Square.rank to_) / 2)

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
