(** The two players. *)

type t =
  | White
  | Black

let opposite = function White -> Black | Black -> White
let equal c1 c2 = c1 = c2
let to_string = function White -> "white" | Black -> "black"
