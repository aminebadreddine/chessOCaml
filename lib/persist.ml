(** Saving and loading games. Owner: Amine. Backup: Yedidia. *)

let to_string (_g : Game.t) : string =
  failwith "Persist.to_string: not implemented yet"

let of_string (_s : string) : (Game.t, string) result =
  failwith "Persist.of_string: not implemented yet"

let save (_path : string) (_g : Game.t) : (unit, string) result =
  failwith "Persist.save: not implemented yet"

let load (_path : string) : (Game.t, string) result =
  failwith "Persist.load: not implemented yet"
