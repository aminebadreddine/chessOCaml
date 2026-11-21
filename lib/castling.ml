(** Castling rights.

    Representation: a four-bit int, one bit per right. *)

type side =
  | Kingside
  | Queenside

type t = int

let bit color side =
  match (color, side) with
  | Color.White, Kingside -> 1
  | Color.White, Queenside -> 2
  | Color.Black, Kingside -> 4
  | Color.Black, Queenside -> 8

let none = 0
let all = 15
let mem t color side = t land bit color side <> 0
let add t color side = t lor bit color side
let remove t color side = t land lnot (bit color side)
let remove_color t color = remove (remove t color Kingside) color Queenside
let equal = Int.equal

let to_string t =
  if t = none then "-"
  else
    let buf = Buffer.create 4 in
    if mem t Color.White Kingside then Buffer.add_char buf 'K';
    if mem t Color.White Queenside then Buffer.add_char buf 'Q';
    if mem t Color.Black Kingside then Buffer.add_char buf 'k';
    if mem t Color.Black Queenside then Buffer.add_char buf 'q';
    Buffer.contents buf

let of_string s =
  if s = "-" then Some none
  else if s = "" then None
  else
    let step acc c =
      match (acc, c) with
      | None, _ -> None
      | Some t, 'K' -> Some (add t Color.White Kingside)
      | Some t, 'Q' -> Some (add t Color.White Queenside)
      | Some t, 'k' -> Some (add t Color.Black Kingside)
      | Some t, 'q' -> Some (add t Color.Black Queenside)
      | Some _, _ -> None
    in
    String.fold_left step (Some none) s
