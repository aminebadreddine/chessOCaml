(** A move, as a request to move a piece from one square to another. *)

type t = {
  from : Square.t;
  to_ : Square.t;
  promotion : Piece.kind option;
}

let make ?promotion from to_ = { from; to_; promotion }
let equal m1 m2 = m1 = m2

let compare m1 m2 =
  match Square.compare m1.from m2.from with
  | 0 -> (
      match Square.compare m1.to_ m2.to_ with
      | 0 -> Stdlib.compare m1.promotion m2.promotion
      | c -> c)
  | c -> c

let to_string m =
  Square.to_string m.from ^ Square.to_string m.to_
  ^
  match m.promotion with
  | None -> ""
  | Some k -> String.make 1 (Char.lowercase_ascii (Piece.kind_to_char k))

let of_string s =
  let len = String.length s in
  if len <> 4 && len <> 5 then None
  else
    match
      (Square.of_string (String.sub s 0 2), Square.of_string (String.sub s 2 2))
    with
    | Some from, Some to_ ->
        if len = 4 then Some { from; to_; promotion = None }
        else (
          match Piece.kind_of_char s.[4] with
          | Some (Piece.Pawn | Piece.King) | None -> None
          | Some k -> Some { from; to_; promotion = Some k })
    | _ -> None
