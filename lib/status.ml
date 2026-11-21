(** How a game stands. *)

type draw_reason =
  | Stalemate
  | Fifty_move
  | Threefold_repetition
  | Insufficient_material
  | Agreement

type t =
  | Ongoing
  | Check of Color.t
  | Checkmate of Color.t
  | Draw of draw_reason

let is_over = function
  | Ongoing | Check _ -> false
  | Checkmate _ | Draw _ -> true

let reason_to_string = function
  | Stalemate -> "stalemate"
  | Fifty_move -> "the fifty-move rule"
  | Threefold_repetition -> "threefold repetition"
  | Insufficient_material -> "insufficient material"
  | Agreement -> "agreement"

let to_string = function
  | Ongoing -> "ongoing"
  | Check c -> Color.to_string c ^ " is in check"
  | Checkmate c -> Color.to_string c ^ " is checkmated"
  | Draw r -> "draw by " ^ reason_to_string r
