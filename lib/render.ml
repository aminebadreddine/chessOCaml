(** Drawing the board. Owner: Amine. Backup: Yedidia. *)

(* Starts as the debugging renderer so that the terminal game works from day
   one. Replace with the real thing. *)
let ascii b = Board.to_ascii b

let svg ?last_move (_b : Board.t) : string =
  ignore last_move;
  failwith "Render.svg: not implemented yet"
