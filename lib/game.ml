(** A game: the current position together with everything that led to it.

    Representation: the current position, the position the game started from,
    and the history as a list of (position before the move, move), most recent
    first. Consing onto the front makes playing a move O(1) and makes undo a
    pattern match. *)

type t = {
  current : Position.t;
  start : Position.t;
  past : (Position.t * Move.t) list;
}

let of_position pos = { current = pos; start = pos; past = [] }
let create () = of_position Position.initial
let position g = g.current
let board g = Position.board g.current
let turn g = Position.turn g.current
let ply g = List.length g.past
let history g = g.past
let moves g = List.rev_map snd g.past
let start_position g = g.start

let apply_unchecked g move =
  {
    g with
    current = Position.apply_unchecked g.current move;
    past = (g.current, move) :: g.past;
  }

let undo g =
  match g.past with
  | [] -> None
  | (previous, _) :: rest -> Some { g with current = previous; past = rest }

let repetition_count g =
  let same pos = Position.same_position pos g.current in
  (* The current position counts once, plus every earlier occurrence. The
     stored positions are the ones before each move, so the whole list is
     exactly the set of earlier positions. *)
  1 + List.length (List.filter (fun (pos, _) -> same pos) g.past)

let is_fifty_move_draw g = Position.halfmove_clock g.current >= 100
