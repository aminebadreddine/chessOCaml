(** Computer opponent. Owner: Jah. Backup: Shehroze. *)

type strategy =
  | Random
  | Greedy_capture
  | Avoid_threats

let piece_value : Piece.kind -> int = function
  | Piece.Pawn -> 100
  | Piece.Knight -> 320
  | Piece.Bishop -> 330
  | Piece.Rook -> 500
  | Piece.Queen -> 900
  | Piece.King -> 0

let evaluate (pos : Position.t) : int =
  List.fold_left
    (fun total (_, (p : Piece.t)) ->
      match p.color with
      | Color.White -> total + piece_value p.kind
      | Color.Black -> total - piece_value p.kind)
    0
    (Board.to_list (Position.board pos))

(* [evaluate] is from White's side; flip it so higher is always better for
   [color]. *)
let score_for (color : Color.t) (pos : Position.t) : int =
  match color with
  | Color.White -> evaluate pos
  | Color.Black -> -evaluate pos

(* Material the mover gains going from [pos] to [after]. Comparing
   whole-board scores means captures, en passant, and promotion need no
   special cases. *)
let material_gain (pos : Position.t) (after : Position.t) : int =
  let me = Position.turn pos in
  score_for me after - score_for me pos

(* The most valuable piece of ours the opponent could legally capture in
   [after], where it is the opponent's turn. This covers every piece, not
   just the one that moved, so a move that leaves an already attacked piece
   where it is gets charged for it. Asking [Rules] rather than
   [Movegen.attacks] matters: a piece pinned to its own king attacks squares
   it cannot actually move to, and a king cannot take a defended piece. *)
let threatened_value (after : Position.t) : int =
  let them = Position.turn after in
  List.fold_left
    (fun worst (m : Move.t) ->
      match Position.piece_at after m.to_ with
      | Some p when not (Color.equal p.color them) ->
          max worst (piece_value p.kind)
      | _ -> worst)
    0
    (Rules.legal_moves after)

let score (strategy : strategy) (pos : Position.t) (m : Move.t) : int =
  match strategy with
  | Random -> 0
  | Greedy_capture -> material_gain pos (Position.apply_unchecked pos m)
  | Avoid_threats ->
      let after = Position.apply_unchecked pos m in
      material_gain pos after - threatened_value after

let pick (rng : Random.State.t) (xs : 'a list) : 'a =
  List.nth xs (Random.State.int rng (List.length xs))

(* Created on first use so that loading the library does not seed anything. *)
let default_rng = lazy (Random.State.make_self_init ())

let choose ?(strategy = Avoid_threats) ?rng (g : Game.t) : Move.t option =
  let rng = match rng with Some r -> r | None -> Lazy.force default_rng in
  let pos = Game.position g in
  match Engine.legal_moves g with
  | [] -> None
  | moves ->
      let scored = List.map (fun m -> (score strategy pos m, m)) moves in
      let best = List.fold_left (fun acc (s, _) -> max acc s) min_int scored in
      let ties =
        List.filter_map (fun (s, m) -> if s = best then Some m else None) scored
      in
      Some (pick rng ties)
