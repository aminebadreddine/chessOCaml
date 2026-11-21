(** Test suite for the chess engine foundation.

    No external test library, so a fresh clone runs [dune test] with nothing
    installed. If the team later adds OUnit2, the checks below translate
    directly; the harness is the only part that changes.

    Two groups. The {b foundation} group covers what is implemented and must
    stay green: anyone whose change turns one of these red has broken
    something someone else depends on. The {b staged} group covers the special
    rules that are deliberately not implemented yet. Those failures are the
    to-do list, not a broken build, so they do not fail [dune test]. Move a
    test out of the staged group as its feature lands. *)

open ChessOCaml

(* ------------------------------------------------------------------ *)
(* Harness                                                             *)
(* ------------------------------------------------------------------ *)

let foundation_failures = ref 0
let staged_failures = ref 0
let checks = ref 0
let group = ref `Foundation

let fail name detail =
  (match !group with
  | `Foundation -> incr foundation_failures
  | `Staged -> incr staged_failures);
  Printf.printf "  FAIL  %s\n        %s\n" name detail

let check name condition =
  incr checks;
  if not condition then fail name "expected this to hold"

(* Comparing by printed form keeps failures readable without a pretty-printer
   for every type. *)
let check_string name ~expected ~actual =
  incr checks;
  if expected <> actual then
    fail name (Printf.sprintf "expected %S but got %S" expected actual)

let check_int name ~expected ~actual =
  incr checks;
  if expected <> actual then
    fail name (Printf.sprintf "expected %d but got %d" expected actual)

let section name =
  Printf.printf "%s\n" name

(* Parsing helpers. A test that names a bad square is a broken test, so these
   raise rather than returning an option. *)
let sq name =
  match Square.of_string name with
  | Some s -> s
  | None -> failwith ("test bug: no square called " ^ name)

let mv text =
  match Move.of_string text with
  | Some m -> m
  | None -> failwith ("test bug: cannot read move " ^ text)

let play pos text = Position.apply_unchecked pos (mv text)
let play_all pos texts = List.fold_left play pos texts
let fen pos = Fen.of_position pos
let at pos name =
  match Position.piece_at pos (sq name) with
  | None -> "-"
  | Some p -> String.make 1 (Piece.to_char p)

(* ------------------------------------------------------------------ *)
(* Foundation: squares                                                 *)
(* ------------------------------------------------------------------ *)

let test_squares () =
  section "squares";
  check_int "e4 is on file 4" ~expected:4 ~actual:(Square.file (sq "e4"));
  check_int "e4 is on rank 3" ~expected:3 ~actual:(Square.rank (sq "e4"));
  check_string "a1 round-trips" ~expected:"a1"
    ~actual:(Square.to_string (sq "a1"));
  check_string "h8 round-trips" ~expected:"h8"
    ~actual:(Square.to_string (sq "h8"));
  check_int "a1 is index 0" ~expected:0 ~actual:(Square.to_index (sq "a1"));
  check_int "h8 is index 63" ~expected:63 ~actual:(Square.to_index (sq "h8"));
  check "there are 64 squares" (List.length Square.all = 64);
  check "j9 is not a square" (Square.of_string "j9" = None);
  check "a short string is not a square" (Square.of_string "e" = None);
  check "shifting off the a-file fails"
    (Square.shift (sq "a1") ~df:(-1) ~dr:0 = None);
  check "shifting off the eighth rank fails"
    (Square.shift (sq "e8") ~df:0 ~dr:1 = None);
  match Square.shift (sq "e4") ~df:1 ~dr:2 with
  | Some s -> check_string "a knight hop from e4" ~expected:"f6"
                ~actual:(Square.to_string s)
  | None -> fail "a knight hop from e4" "went off the board"

(* ------------------------------------------------------------------ *)
(* Foundation: pieces, moves, castling rights                          *)
(* ------------------------------------------------------------------ *)

let test_pieces () =
  section "pieces";
  let all_kinds =
    [ Piece.Pawn; Piece.Knight; Piece.Bishop; Piece.Rook; Piece.Queen;
      Piece.King ]
  in
  List.iter
    (fun color ->
      List.iter
        (fun kind ->
          let p = Piece.make color kind in
          match Piece.of_char (Piece.to_char p) with
          | Some p' ->
              check ("round-trip " ^ Piece.to_string p) (Piece.equal p p')
          | None -> fail ("round-trip " ^ Piece.to_string p) "did not parse")
        all_kinds)
    [ Color.White; Color.Black ];
  check "a white knight is N" (Piece.to_char (Piece.make Color.White Piece.Knight) = 'N');
  check "a black pawn is p" (Piece.to_char (Piece.make Color.Black Piece.Pawn) = 'p');
  check "x is not a piece" (Piece.of_char 'x' = None)

let test_moves () =
  section "moves";
  check_string "a quiet move prints" ~expected:"e2e4"
    ~actual:(Move.to_string (mv "e2e4"));
  check_string "a promotion prints" ~expected:"e7e8q"
    ~actual:(Move.to_string (mv "e7e8q"));
  check "promoting to a king is not a move" (Move.of_string "e7e8k" = None);
  check "promoting to a pawn is not a move" (Move.of_string "e7e8p" = None);
  check "a three-character move is not a move" (Move.of_string "e2e" = None);
  check "moves compare by value" (Move.equal (mv "e2e4") (mv "e2e4"))

let test_castling_rights () =
  section "castling rights";
  check_string "all rights print as KQkq" ~expected:"KQkq"
    ~actual:(Castling.to_string Castling.all);
  check_string "no rights print as a dash" ~expected:"-"
    ~actual:(Castling.to_string Castling.none);
  let only_white_king =
    Castling.add Castling.none Color.White Castling.Kingside
  in
  check_string "one right prints alone" ~expected:"K"
    ~actual:(Castling.to_string only_white_king);
  check "white may castle kingside"
    (Castling.mem only_white_king Color.White Castling.Kingside);
  check "white may not castle queenside"
    (not (Castling.mem only_white_king Color.White Castling.Queenside));
  let after_king_move = Castling.remove_color Castling.all Color.White in
  check_string "a king move costs both white rights" ~expected:"kq"
    ~actual:(Castling.to_string after_king_move);
  (match Castling.of_string "KQkq" with
  | Some c -> check "KQkq parses" (Castling.equal c Castling.all)
  | None -> fail "KQkq parses" "returned None");
  (match Castling.of_string "-" with
  | Some c -> check "a dash parses" (Castling.equal c Castling.none)
  | None -> fail "a dash parses" "returned None");
  check "junk does not parse" (Castling.of_string "Kx" = None)

(* ------------------------------------------------------------------ *)
(* Foundation: the board                                               *)
(* ------------------------------------------------------------------ *)

let test_board () =
  section "board";
  let b = Board.initial in
  check_string "white king starts on e1" ~expected:"K"
    ~actual:
      (match Board.get b (sq "e1") with
      | Some p -> String.make 1 (Piece.to_char p)
      | None -> "-");
  check_string "black queen starts on d8" ~expected:"q"
    ~actual:
      (match Board.get b (sq "d8") with
      | Some p -> String.make 1 (Piece.to_char p)
      | None -> "-");
  check "e4 starts empty" (Board.is_empty b (sq "e4"));
  check_int "thirty-two pieces at the start" ~expected:32
    ~actual:(List.length (Board.to_list b));
  check_int "sixteen white pieces" ~expected:16
    ~actual:(List.length (Board.pieces_of b Color.White));
  check "e2 is white's" (Board.occupied_by b (sq "e2") Color.White);
  check "e2 is not black's" (not (Board.occupied_by b (sq "e2") Color.Black));
  (match Board.find_king b Color.Black with
  | Some s -> check_string "black's king is found" ~expected:"e8"
                ~actual:(Square.to_string s)
  | None -> fail "black's king is found" "no king on the board");
  check "an empty board has no king"
    (Board.find_king Board.empty Color.White = None);
  (* The invariant the whole design rests on. *)
  let b' = Board.set b (sq "e1") None in
  check "setting a square leaves the original alone"
    (Board.get b (sq "e1") <> None);
  check "setting a square changes the copy" (Board.get b' (sq "e1") = None);
  check "boards compare by contents" (Board.equal Board.initial b);
  check "a changed board is not equal" (not (Board.equal b b'));
  let moved = Board.move_piece b ~from:(sq "e2") ~to_:(sq "e4") in
  check "the origin empties" (Board.is_empty moved (sq "e2"));
  check "the destination fills" (not (Board.is_empty moved (sq "e4")))

(* ------------------------------------------------------------------ *)
(* Foundation: positions and ordinary moves                            *)
(* ------------------------------------------------------------------ *)

let initial_fen =
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let test_position () =
  section "positions";
  check_string "the starting position has the standard FEN"
    ~expected:initial_fen ~actual:(fen Position.initial);
  check "white moves first"
    (Color.equal (Position.turn Position.initial) Color.White);
  check_int "the clock starts at zero" ~expected:0
    ~actual:(Position.halfmove_clock Position.initial);
  check_int "the move number starts at one" ~expected:1
    ~actual:(Position.fullmove_number Position.initial);
  check "there is no en passant target at the start"
    (Position.en_passant Position.initial = None)

let test_ordinary_moves () =
  section "ordinary moves";
  let after_e4 = play Position.initial "e2e4" in
  check_string "the pawn arrives on e4" ~expected:"P" ~actual:(at after_e4 "e4");
  check_string "e2 is left empty" ~expected:"-" ~actual:(at after_e4 "e2");
  check "the turn passes to black"
    (Color.equal (Position.turn after_e4) Color.Black);
  check_int "a pawn move resets the clock" ~expected:0
    ~actual:(Position.halfmove_clock after_e4);
  check_int "the move number waits for black" ~expected:1
    ~actual:(Position.fullmove_number after_e4);
  let after_e5 = play after_e4 "e7e5" in
  check_int "the move number advances after black" ~expected:2
    ~actual:(Position.fullmove_number after_e5);
  let after_nf3 = play after_e5 "g1f3" in
  check_int "a piece move advances the clock" ~expected:1
    ~actual:(Position.halfmove_clock after_nf3);
  let after_nxe5 = play after_nf3 "f3e5" in
  check_string "the knight takes the pawn" ~expected:"N"
    ~actual:(at after_nxe5 "e5");
  check_int "a capture resets the clock" ~expected:0
    ~actual:(Position.halfmove_clock after_nxe5);
  check_int "thirty-one pieces remain" ~expected:31
    ~actual:(List.length (Board.to_list (Position.board after_nxe5)));
  check "playing from an empty square is rejected"
    (match play Position.initial "e3e4" with
    | exception Invalid_argument _ -> true
    | _ -> false)

(* ------------------------------------------------------------------ *)
(* Foundation: games and history                                       *)
(* ------------------------------------------------------------------ *)

let test_game () =
  section "games and history";
  let g = Game.create () in
  check_int "a new game is at ply zero" ~expected:0 ~actual:(Game.ply g);
  check "a new game cannot be undone" (Game.undo g = None);
  let g = Game.apply_unchecked g (mv "e2e4") in
  let g = Game.apply_unchecked g (mv "e7e5") in
  check_int "two moves make two plies" ~expected:2 ~actual:(Game.ply g);
  check_string "the moves are kept in order" ~expected:"e2e4 e7e5"
    ~actual:(String.concat " " (List.map Move.to_string (Game.moves g)));
  check_string "the game remembers where it began" ~expected:initial_fen
    ~actual:(fen (Game.start_position g));
  (match Game.undo g with
  | None -> fail "undo steps back" "nothing to undo"
  | Some g' ->
      check_int "undo drops a ply" ~expected:1 ~actual:(Game.ply g');
      check_string "undo restores the earlier position"
        ~expected:(fen (play Position.initial "e2e4"))
        ~actual:(fen (Game.position g'));
      check "undo does not disturb the newer game"
        (Game.ply g = 2));
  (* Knights out and back reaches the starting position a second time. *)
  let repeated =
    List.fold_left
      (fun g text -> Game.apply_unchecked g (mv text))
      (Game.create ())
      [ "g1f3"; "g8f6"; "f3g1"; "f6g8" ]
  in
  check_int "a repeated position is counted twice" ~expected:2
    ~actual:(Game.repetition_count repeated);
  check_int "an unrepeated position is counted once" ~expected:1
    ~actual:(Game.repetition_count (Game.create ()));
  check "fifty moves have not passed"
    (not (Game.is_fifty_move_draw (Game.create ())))

(* ------------------------------------------------------------------ *)
(* Foundation: FEN printing                                            *)
(* ------------------------------------------------------------------ *)

let test_fen_printing () =
  section "FEN printing";
  check_string "the starting FEN is the standard one" ~expected:initial_fen
    ~actual:Fen.initial;
  check_string "an empty board prints as eight eights"
    ~expected:"8/8/8/8/8/8/8/8" ~actual:(Fen.of_board Board.empty);
  let after = play_all Position.initial [ "e2e4"; "c7c5" ] in
  check_string "a played position prints"
    ~expected:"rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
    ~actual:(fen after)

(* ------------------------------------------------------------------ *)
(* Staged: the special rules, not implemented yet                      *)
(* ------------------------------------------------------------------ *)

let test_en_passant () =
  section "staged: en passant";
  let p = play_all Position.initial [ "e2e4"; "a7a6"; "e4e5"; "d7d5" ] in
  check_string "a double push sets the target square" ~expected:"d6"
    ~actual:
      (match Position.en_passant p with
      | Some s -> Square.to_string s
      | None -> "-");
  let p2 = play p "e5d6" in
  check_string "the capturing pawn lands behind" ~expected:"P"
    ~actual:(at p2 "d6");
  check_string "the captured pawn is removed from d5" ~expected:"-"
    ~actual:(at p2 "d5");
  let quiet = play_all Position.initial [ "e2e4"; "a7a6" ] in
  check "any other move clears the target"
    (Position.en_passant quiet = None)

let test_promotion () =
  section "staged: promotion";
  let board =
    Board.set
      (Board.set
         (Board.set Board.empty (sq "a7") (Some (Piece.make Color.White Piece.Pawn)))
         (sq "e1")
         (Some (Piece.make Color.White Piece.King)))
      (sq "e8")
      (Some (Piece.make Color.Black Piece.King))
  in
  let pos =
    Position.make ~board ~turn:Color.White ~castling:Castling.none
      ~en_passant:None ~halfmove_clock:0 ~fullmove_number:1
  in
  let promoted = play pos "a7a8q" in
  check_string "the pawn becomes a queen" ~expected:"Q" ~actual:(at promoted "a8");
  check_string "the pawn leaves the seventh rank" ~expected:"-"
    ~actual:(at promoted "a7")

let test_castling () =
  section "staged: castling";
  let board =
    List.fold_left
      (fun b (name, piece) -> Board.set b (sq name) (Some piece))
      Board.empty
      [
        ("e1", Piece.make Color.White Piece.King);
        ("h1", Piece.make Color.White Piece.Rook);
        ("a1", Piece.make Color.White Piece.Rook);
        ("e8", Piece.make Color.Black Piece.King);
        ("h8", Piece.make Color.Black Piece.Rook);
      ]
  in
  let pos =
    Position.make ~board ~turn:Color.White ~castling:Castling.all
      ~en_passant:None ~halfmove_clock:0 ~fullmove_number:1
  in
  let short = play pos "e1g1" in
  check_string "the king reaches g1" ~expected:"K" ~actual:(at short "g1");
  check_string "the rook jumps to f1" ~expected:"R" ~actual:(at short "f1");
  check_string "h1 is vacated" ~expected:"-" ~actual:(at short "h1");
  check_string "castling costs both rights" ~expected:"kq"
    ~actual:(Castling.to_string (Position.castling short));
  let long = play pos "e1c1" in
  check_string "the queenside rook reaches d1" ~expected:"R"
    ~actual:(at long "d1");
  let king_moved = play pos "e1e2" in
  check_string "a king move costs both rights" ~expected:"kq"
    ~actual:(Castling.to_string (Position.castling king_moved));
  let rook_moved = play pos "h1g1" in
  check_string "a rook move costs one right" ~expected:"Qkq"
    ~actual:(Castling.to_string (Position.castling rook_moved));
  let rook_taken = play_all pos [ "h1h8" ] in
  check_string "capturing a rook costs its owner the right" ~expected:"Qq"
    ~actual:(Castling.to_string (Position.castling rook_taken))

let test_fen_parsing () =
  section "staged: FEN parsing";
  (match Fen.to_position initial_fen with
  | Ok p ->
      check "the starting FEN parses back" (Position.equal p Position.initial)
  | Error m -> fail "the starting FEN parses back" m);
  let played = play_all Position.initial [ "e2e4"; "c7c5"; "g1f3" ] in
  (match Fen.to_position (fen played) with
  | Ok p -> check_string "any position round-trips" ~expected:(fen played)
              ~actual:(fen p)
  | Error m -> fail "any position round-trips" m);
  check "a FEN with seven ranks is rejected"
    (match Fen.to_position "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1" with
    | Error _ -> true
    | Ok _ -> false);
  check "an empty string is rejected"
    (match Fen.to_position "" with Error _ -> true | Ok _ -> false)

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let run_guarded name f =
  (* A staged feature may raise rather than return a wrong answer. *)
  try f () with
  | Failure m -> fail name ("raised Failure: " ^ m)
  | Invalid_argument m -> fail name ("raised Invalid_argument: " ^ m)

let () =
  print_endline "=== foundation ===";
  group := `Foundation;
  List.iter
    (fun (name, f) -> run_guarded name f)
    [
      ("squares", test_squares);
      ("pieces", test_pieces);
      ("moves", test_moves);
      ("castling rights", test_castling_rights);
      ("board", test_board);
      ("positions", test_position);
      ("ordinary moves", test_ordinary_moves);
      ("games", test_game);
      ("FEN printing", test_fen_printing);
    ];
  print_endline "\n=== staged features (not implemented yet) ===";
  group := `Staged;
  List.iter
    (fun (name, f) -> run_guarded name f)
    [
      ("en passant", test_en_passant);
      ("promotion", test_promotion);
      ("castling", test_castling);
      ("FEN parsing", test_fen_parsing);
    ];
  Printf.printf "\n%d checks, %d foundation failures, %d staged failures\n"
    !checks !foundation_failures !staged_failures;
  if !foundation_failures > 0 then (
    print_endline "FOUNDATION BROKEN: something everyone depends on is wrong.";
    exit 1)
  else if !staged_failures > 0 then
    Printf.printf
      "Foundation is green. %d staged checks are waiting on the special \
       rules.\n"
      !staged_failures
  else print_endline "All green."
