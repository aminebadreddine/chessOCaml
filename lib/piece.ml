(** Chess pieces: a kind together with the player who owns it. *)

type kind =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type t = {
  color : Color.t;
  kind : kind;
}

let make color kind = { color; kind }
let equal p1 p2 = p1 = p2

let kind_to_char = function
  | Pawn -> 'P'
  | Knight -> 'N'
  | Bishop -> 'B'
  | Rook -> 'R'
  | Queen -> 'Q'
  | King -> 'K'

let kind_of_char c =
  match Char.uppercase_ascii c with
  | 'P' -> Some Pawn
  | 'N' -> Some Knight
  | 'B' -> Some Bishop
  | 'R' -> Some Rook
  | 'Q' -> Some Queen
  | 'K' -> Some King
  | _ -> None

let to_char p =
  let c = kind_to_char p.kind in
  match p.color with Color.White -> c | Color.Black -> Char.lowercase_ascii c

let of_char c =
  match kind_of_char c with
  | None -> None
  | Some kind ->
      let color =
        if Char.uppercase_ascii c = c then Color.White else Color.Black
      in
      Some { color; kind }

let kind_to_string = function
  | Pawn -> "pawn"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"

let to_string p = Color.to_string p.color ^ " " ^ kind_to_string p.kind
