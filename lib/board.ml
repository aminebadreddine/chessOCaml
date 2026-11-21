(** The placement of pieces on the 64 squares.

    Representation: an array of 64 [Piece.t option], indexed by
    [Square.to_index], so index 0 is a1 and index 63 is h8.

    Invariant: the array behind a value of type [t] is never mutated after that
    value is built. Every operation that changes a board copies first. Breaking
    this makes two "different" boards share storage, which silently corrupts
    game history. *)

type t = Piece.t option array

let empty = Array.make 64 None
let get b sq = b.(Square.to_index sq)

let set b sq p =
  let b' = Array.copy b in
  b'.(Square.to_index sq) <- p;
  b'

let move_piece b ~from ~to_ =
  let b' = Array.copy b in
  let i = Square.to_index from and j = Square.to_index to_ in
  b'.(j) <- b'.(i);
  b'.(i) <- None;
  b'

let is_empty b sq = get b sq = None

let occupied_by b sq color =
  match get b sq with
  | Some p -> Color.equal p.Piece.color color
  | None -> false

let fold f b init =
  let acc = ref init in
  for i = 0 to 63 do
    match Square.of_index i with
    | Some sq -> acc := f sq b.(i) !acc
    | None -> ()
  done;
  !acc

let to_list b =
  List.rev
    (fold
       (fun sq p acc -> match p with Some p -> (sq, p) :: acc | None -> acc)
       b [])

let pieces_of b color =
  List.filter (fun (_, p) -> Color.equal p.Piece.color color) (to_list b)

let find_king b color =
  let is_king (_, p) =
    Color.equal p.Piece.color color && p.Piece.kind = Piece.King
  in
  match List.find_opt is_king (to_list b) with
  | Some (sq, _) -> Some sq
  | None -> None

let equal b1 b2 =
  let rec go i =
    i > 63 || (b1.(i) = b2.(i) && go (i + 1))
  in
  go 0

(* The starting arrangement of the non-pawn ranks, a-file to h-file. *)
let back_rank =
  [|
    Piece.Rook;
    Piece.Knight;
    Piece.Bishop;
    Piece.Queen;
    Piece.King;
    Piece.Bishop;
    Piece.Knight;
    Piece.Rook;
  |]

let initial =
  let b = Array.make 64 None in
  let put ~file ~rank piece =
    match Square.make ~file ~rank with
    | Some sq -> b.(Square.to_index sq) <- Some piece
    | None -> ()
  in
  for file = 0 to 7 do
    put ~file ~rank:0 (Piece.make Color.White back_rank.(file));
    put ~file ~rank:1 (Piece.make Color.White Piece.Pawn);
    put ~file ~rank:6 (Piece.make Color.Black Piece.Pawn);
    put ~file ~rank:7 (Piece.make Color.Black back_rank.(file))
  done;
  b

let to_ascii b =
  let buf = Buffer.create 160 in
  for rank = 7 downto 0 do
    Buffer.add_string buf (string_of_int (rank + 1));
    Buffer.add_char buf ' ';
    for file = 0 to 7 do
      let c =
        match Square.make ~file ~rank with
        | Some sq -> (
            match get b sq with Some p -> Piece.to_char p | None -> '.')
        | None -> '?'
      in
      Buffer.add_char buf c;
      Buffer.add_char buf ' '
    done;
    Buffer.add_char buf '\n'
  done;
  Buffer.add_string buf "  a b c d e f g h\n";
  Buffer.contents buf
