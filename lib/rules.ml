(** Legality and game outcome. Owner: Shehroze. Backup: Jah. *)

(* is [color]'s king attacked rn. no king on the board (some test
   positions) just counts as not in check *)
let is_in_check (pos : Position.t) (color : Color.t) : bool =
  match Board.find_king (Position.board pos) color with
  | None -> false
  | Some king_sq -> Movegen.attacks pos (Color.opposite color) king_sq

(* castling is the one move where checking where the king lands isnt enough.
   you also cant castle out of check or through an attacked square.
   movegen doesnt make castling moves yet but once it does this catches them.
   a castle = the king moving two files *)
let castle_ok (pos : Position.t) (move : Move.t) : bool =
  let me = Position.turn pos in
  match Position.piece_at pos move.from with
  | Some { Piece.kind = Piece.King; _ }
    when abs (Square.file move.to_ - Square.file move.from) = 2 -> (
      let step = if Square.file move.to_ > Square.file move.from then 1 else -1 in
      match Square.shift move.from ~df:step ~dr:0 with
      | None -> false
      | Some middle ->
          (not (is_in_check pos me))
          && not (Movegen.attacks pos (Color.opposite me) middle))
  | _ -> true

(* the actual filter. play the move, and its legal if my king isnt attacked
   after. NOTE: turn has already flipped after apply_unchecked so we grab
   [me] before playing it *)
let keep_legal (pos : Position.t) (moves : Move.t list) : Move.t list =
  let me = Position.turn pos in
  List.filter
    (fun m ->
      castle_ok pos m && not (is_in_check (Position.apply_unchecked pos m) me))
    moves

let legal_moves (pos : Position.t) : Move.t list =
  keep_legal pos (Movegen.pseudo_legal pos)

let legal_moves_from (pos : Position.t) (sq : Square.t) : Move.t list =
  keep_legal pos (Movegen.pseudo_legal_from pos sq)

(* legal = shows up in the legal list. this also throws out junk like a1a8
   that isnt even pseudo legal. only gen moves from that one square so its
   not slow *)
let is_legal (pos : Position.t) (move : Move.t) : bool =
  List.exists (Move.equal move) (legal_moves_from pos move.from)

(* only kings left, or kings + one bishop/knight total *)
let is_insufficient_material (pos : Position.t) : bool =
  let others =
    List.filter
      (fun (_, (p : Piece.t)) -> p.kind <> Piece.King)
      (Board.to_list (Position.board pos))
  in
  match others with
  | [] -> true
  | [ (_, (p : Piece.t)) ] -> p.kind = Piece.Bishop || p.kind = Piece.Knight
  | _ -> false

(* order matters: no moves left decides it first (mate or stalemate),
   then the draws, then check *)
let status (g : Game.t) : Status.t =
  let pos = Game.position g in
  let me = Position.turn pos in
  let in_check = is_in_check pos me in
  if legal_moves pos = [] then
    if in_check then Status.Checkmate me else Status.Draw Status.Stalemate
  else if Game.is_fifty_move_draw g then Status.Draw Status.Fifty_move
  else if Game.repetition_count g >= 3 then
    Status.Draw Status.Threefold_repetition
  else if is_insufficient_material pos then
    Status.Draw Status.Insufficient_material
  else if in_check then Status.Check me
  else Status.Ongoing