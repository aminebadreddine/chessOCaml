(** The checked front door to the engine. *)

type error =
  | Illegal_move of Move.t
  | Game_already_over of Status.t
  | Nothing_to_undo

let error_to_string = function
  | Illegal_move m -> "illegal move: " ^ Move.to_string m
  | Game_already_over s -> "the game is over (" ^ Status.to_string s ^ ")"
  | Nothing_to_undo -> "there is no move to take back"

let new_game () = Game.create ()
let status g = Rules.status g
let legal_moves g = Rules.legal_moves (Game.position g)

let play g move =
  let current = status g in
  if Status.is_over current then Error (Game_already_over current)
  else if Rules.is_legal (Game.position g) move then
    Ok (Game.apply_unchecked g move)
  else Error (Illegal_move move)

let undo g =
  match Game.undo g with Some g' -> Ok g' | None -> Error Nothing_to_undo
