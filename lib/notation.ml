(** Parsing and printing moves. Owner: Amine. Backup: Yedidia. *)

type error =
  | Malformed of string
  | No_such_move of string
  | Ambiguous of string * Move.t list

let error_to_string = function
  | Malformed s -> "I could not read \"" ^ s ^ "\" as a move"
  | No_such_move s -> "\"" ^ s ^ "\" is not a legal move here"
  | Ambiguous (s, _) -> "\"" ^ s ^ "\" could mean more than one move"

let parse (_pos : Position.t) (_s : string) : (Move.t, error) result =
  failwith "Notation.parse: not implemented yet"

let to_san (_pos : Position.t) (_move : Move.t) : string =
  failwith "Notation.to_san: not implemented yet"
