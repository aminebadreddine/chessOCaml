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
(* TODO. This is the one piece of the foundation left to write, and it *)
(* is the same shape as the A1 parser: split the input, validate each  *)
(* field, and return a typed value or an error message. Suggested      *)
(* decomposition, each part small enough to test on its own:           *)
(*                                                                     *)
(*   board_of_string : string -> (Board.t, string) result              *)
(*     Split on '/' into exactly 8 rank strings, rank 8 first. For      *)
(*     each, walk the characters: a digit advances the file by that    *)
(*     many squares, a letter places Piece.of_char and advances one.   *)
(*     Error if a rank does not end on file 8.                          *)
(*                                                                     *)
(*   turn_of_string   : "w" | "b"                                       *)
(*   ep_of_string     : "-" or a square                                 *)
(*   int_field        : a non-negative integer                          *)
(*                                                                     *)
(* Castling already parses: Castling.of_string.                         *)
(*                                                                     *)
(* Check it against of_position: for any position p,                    *)
(*   to_position (of_position p) = Ok p                                 *)
(* That round-trip is the whole test suite for this function, and it    *)
(* is already written in test/test_chessOCaml.ml.                       *)
(* ------------------------------------------------------------------ *)

let to_position (_s : string) : (Position.t, string) result =
  Error "Fen.to_position: not implemented yet"
