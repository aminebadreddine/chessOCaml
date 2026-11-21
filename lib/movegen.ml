(** Pseudo-legal move generation. Owner: Xavier. Backup: Amine. *)

let todo name = failwith ("Movegen." ^ name ^ ": not implemented yet")
let pseudo_legal (_pos : Position.t) : Move.t list = todo "pseudo_legal"

let pseudo_legal_from (_pos : Position.t) (_sq : Square.t) : Move.t list =
  todo "pseudo_legal_from"

let attacks (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : bool =
  todo "attacks"

let attackers (_pos : Position.t) (_color : Color.t) (_sq : Square.t) :
    Square.t list =
  todo "attackers"
