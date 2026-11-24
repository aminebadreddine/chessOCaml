(** Forsyth-Edwards Notation. *)

let of_board b =
  let buf = Buffer.create 72 in
  for rank = 7 downto 0 do
    let run = ref 0 in
    let flush () =
      if !run > 0 then (
        Buffer.add_string buf (string_of_int !run);
        run := 0)
    in
    for file = 0 to 7 do
      match Square.make ~file ~rank with
      | None -> ()
      | Some sq -> (
          match Board.get b sq with
          | None -> incr run
          | Some p ->
              flush ();
              Buffer.add_char buf (Piece.to_char p))
    done;
    flush ();
    if rank > 0 then Buffer.add_char buf '/'
  done;
  Buffer.contents buf

let of_position pos =
  String.concat " "
    [
      of_board (Position.board pos);
      (match Position.turn pos with Color.White -> "w" | Color.Black -> "b");
      Castling.to_string (Position.castling pos);
      (match Position.en_passant pos with
      | None -> "-"
      | Some sq -> Square.to_string sq);
      string_of_int (Position.halfmove_clock pos);
      string_of_int (Position.fullmove_number pos);
    ]

let initial = of_position Position.initial

(* ------------------------------------------------------------------ *)
(* Parsing                                                             *)
(*                                                                     *)
(* Same shape as the A1 parser: split the input, validate each field   *)
(* on its own, and return a typed value or a message saying what was   *)
(* wrong. Each field parser below is small enough to test by itself.   *)
(* ------------------------------------------------------------------ *)

(* [let*] threads failure through a sequence of steps: if the right-hand side
   is an [Error], everything after it is skipped and that error is the result.
   Without it each field would need its own nested [match], and the six of
   them would nest six deep. This is [Result.bind] written so it reads like an
   ordinary [let]. *)
let ( let* ) = Result.bind

(* One rank of the placement field, left to right, onto [board]. [rank] is the
   rank index, so the first rank string of a FEN carries rank 7. *)
let place_rank board ~rank text =
  let length = String.length text in
  let rec step board file i =
    if i >= length then
      if file = 8 then Ok board
      else
        Error
          (Printf.sprintf "rank %d describes %d squares, not 8" (rank + 1) file)
    else
      let c = text.[i] in
      if c >= '1' && c <= '8' then
        let skipped = Char.code c - Char.code '0' in
        if file + skipped > 8 then
          Error (Printf.sprintf "rank %d runs past the h-file" (rank + 1))
        else step board (file + skipped) (i + 1)
      else
        match (Piece.of_char c, Square.make ~file ~rank) with
        | Some piece, Some sq ->
            step (Board.set board sq (Some piece)) (file + 1) (i + 1)
        | None, _ -> Error (Printf.sprintf "%C is not a piece letter" c)
        | _, None ->
            Error (Printf.sprintf "rank %d runs past the h-file" (rank + 1))
  in
  step board 0 0

let board_of_string placement =
  let ranks = String.split_on_char '/' placement in
  if List.length ranks <> 8 then
    Error
      (Printf.sprintf "piece placement needs 8 ranks separated by /, got %d"
         (List.length ranks))
  else
    (* A FEN lists rank 8 first, so the nth string describes rank 7 - n. *)
    let numbered = List.mapi (fun n text -> (7 - n, text)) ranks in
    List.fold_left
      (fun board (rank, text) ->
        let* board = board in
        place_rank board ~rank text)
      (Ok Board.empty) numbered

let turn_of_string = function
  | "w" -> Ok Color.White
  | "b" -> Ok Color.Black
  | s -> Error (Printf.sprintf "side to move must be w or b, not %S" s)

let castling_of_string s =
  match Castling.of_string s with
  | Some c -> Ok c
  | None -> Error (Printf.sprintf "%S is not a castling field" s)

let en_passant_of_string = function
  | "-" -> Ok None
  | s -> (
      match Square.of_string s with
      | Some sq -> Ok (Some sq)
      | None ->
          Error (Printf.sprintf "%S is not an en passant target square" s))

let count_of_string what s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> Ok n
  | _ -> Error (Printf.sprintf "%s must be a whole number, not %S" what s)

let to_position s =
  let fields =
    String.split_on_char ' ' (String.trim s)
    |> List.filter (fun field -> field <> "")
  in
  match fields with
  | [ placement; turn; castling; en_passant; halfmove; fullmove ] ->
      let* board = board_of_string placement in
      let* turn = turn_of_string turn in
      let* castling = castling_of_string castling in
      let* en_passant = en_passant_of_string en_passant in
      let* halfmove_clock = count_of_string "the halfmove clock" halfmove in
      let* fullmove_number = count_of_string "the move number" fullmove in
      Ok
        (Position.make ~board ~turn ~castling ~en_passant ~halfmove_clock
           ~fullmove_number)
  | _ ->
      Error
        (Printf.sprintf "FEN has six space-separated fields, got %d"
           (List.length fields))
