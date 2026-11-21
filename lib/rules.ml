(** Legality and game outcome. Owner: Shehroze. Backup: Jah. *)

let todo name = failwith ("Rules." ^ name ^ ": not implemented yet")

let is_in_check (_pos : Position.t) (_color : Color.t) : bool =
  todo "is_in_check"

let is_legal (_pos : Position.t) (_move : Move.t) : bool = todo "is_legal"
let legal_moves (_pos : Position.t) : Move.t list = todo "legal_moves"

let legal_moves_from (_pos : Position.t) (_sq : Square.t) : Move.t list =
  todo "legal_moves_from"

let status (_g : Game.t) : Status.t = todo "status"

let is_insufficient_material (_pos : Position.t) : bool =
  todo "is_insufficient_material"
