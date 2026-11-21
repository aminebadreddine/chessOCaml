(** The 64 squares of the board.

    Representation: an [int] in 0..63 equal to [rank * 8 + file]. The invariant
    that every value of type [t] is in range is established by {!make} and
    {!of_index} and preserved by every other function here. *)

type t = int

let make ~file ~rank =
  if file < 0 || file > 7 || rank < 0 || rank > 7 then None
  else Some ((rank * 8) + file)

let file sq = sq mod 8
let rank sq = sq / 8
let of_index i = if i < 0 || i > 63 then None else Some i
let to_index sq = sq

let of_string s =
  if String.length s <> 2 then None
  else
    let f = Char.code (Char.lowercase_ascii s.[0]) - Char.code 'a' in
    let r = Char.code s.[1] - Char.code '1' in
    make ~file:f ~rank:r

let to_string sq =
  Printf.sprintf "%c%d" (Char.chr (Char.code 'a' + file sq)) (rank sq + 1)

let shift sq ~df ~dr = make ~file:(file sq + df) ~rank:(rank sq + dr)
let all = List.init 64 (fun i -> i)
let equal = Int.equal
let compare = Int.compare
