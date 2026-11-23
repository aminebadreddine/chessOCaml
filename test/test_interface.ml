(** Tests for the interface layer: {!Render}, and later [Command] and
    [Session]. Owner: Amine.

    Kept apart from [test_chessOCaml.ml] so that interface work never has to
    edit the engine suite. The harness is a copy of that file's, so failures
    read the same way. Every check here is in the foundation group: the
    interface does not depend on any unimplemented rule. *)

open ChessOCaml

(* ------------------------------------------------------------------ *)
(* Harness                                                             *)
(* ------------------------------------------------------------------ *)

let failures = ref 0
let checks = ref 0

let fail name detail =
  incr failures;
  Printf.printf "  FAIL  %s\n        %s\n" name detail

let check name condition =
  incr checks;
  if not condition then fail name "expected this to hold"

let check_string name ~expected ~actual =
  incr checks;
  if expected <> actual then
    fail name (Printf.sprintf "expected %S but got %S" expected actual)

let check_int name ~expected ~actual =
  incr checks;
  if expected <> actual then
    fail name (Printf.sprintf "expected %d but got %d" expected actual)

let sq name =
  match Square.of_string name with
  | Some s -> s
  | None -> failwith ("test bug: no square called " ^ name)

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

(* [strip_ansi s] is [s] with every escape sequence ESC [ ... m removed, so a
   fancy diagram can be compared by its visible text. *)
let strip_ansi s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec go i =
    if i >= len then ()
    else if s.[i] = '\027' then
      match String.index_from_opt s i 'm' with
      | Some j -> go (j + 1)
      | None -> ()
    else (
      Buffer.add_char buf s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents buf

let lines s = String.split_on_char '\n' s |> List.filter (( <> ) "")

let one_piece name piece =
  Board.set Board.empty (sq name) (Some piece)

(* ------------------------------------------------------------------ *)
(* Render: plain                                                       *)
(* ------------------------------------------------------------------ *)

let initial_white =
  "8 r n b q k b n r \n\
   7 p p p p p p p p \n\
   6 . . . . . . . . \n\
   5 . . . . . . . . \n\
   4 . . . . . . . . \n\
   3 . . . . . . . . \n\
   2 P P P P P P P P \n\
   1 R N B Q K B N R \n\
  \  a b c d e f g h\n"

let initial_black =
  "1 R N B K Q B N R \n\
   2 P P P P P P P P \n\
   3 . . . . . . . . \n\
   4 . . . . . . . . \n\
   5 . . . . . . . . \n\
   6 . . . . . . . . \n\
   7 p p p p p p p p \n\
   8 r n b k q b n r \n\
  \  h g f e d c b a\n"

let test_render_plain () =
  check_string "defaults draw the starting board" ~expected:initial_white
    ~actual:(Render.ascii Board.initial);
  check_string "defaults match Board.to_ascii"
    ~expected:(Board.to_ascii Board.initial)
    ~actual:(Render.ascii Board.initial);
  check_string "an explicit Plain from White is the default"
    ~expected:initial_white
    ~actual:
      (Render.ascii ~style:Render.Plain ~perspective:Color.White Board.initial);
  check_string "from Black, rank 1 is on top and files run h to a"
    ~expected:initial_black
    ~actual:(Render.ascii ~perspective:Color.Black Board.initial);
  let knight = one_piece "g1" (Piece.make Color.White Piece.Knight) in
  check_string "a lone piece lands on its square"
    ~expected:"1 . . . . . . N . "
    ~actual:(List.nth (lines (Render.ascii knight)) 7);
  check_string "from Black the same piece is mirrored"
    ~expected:"1 . N . . . . . . "
    ~actual:(List.hd (lines (Render.ascii ~perspective:Color.Black knight)));
  let targets =
    [ (sq "f3", Render.Target); (sq "h3", Render.Target); (sq "e2", Render.Target) ]
  in
  let board =
    Board.set knight (sq "e2") (Some (Piece.make Color.Black Piece.Pawn))
  in
  let drawn = lines (Render.ascii ~highlights:targets board) in
  check_string "empty targets show as *" ~expected:"3 . . . . . * . * "
    ~actual:(List.nth drawn 5);
  check_string "an occupied target keeps its piece letter"
    ~expected:"2 . . . . p . . . "
    ~actual:(List.nth drawn 6);
  let other =
    [
      (sq "f3", Render.Last_move);
      (sq "h3", Render.Selected);
      (sq "g1", Render.Check);
    ]
  in
  check_string "plain ignores every highlight but Target"
    ~expected:(Render.ascii board)
    ~actual:(Render.ascii ~highlights:other board);
  check_string "a target that also has a stronger highlight still shows *"
    ~expected:"3 . . . . . * . . "
    ~actual:
      (List.nth
         (lines
            (Render.ascii
               ~highlights:[ (sq "f3", Render.Selected); (sq "f3", Render.Target) ]
               Board.empty))
         5);
  check_int "an empty board has nine lines" ~expected:9
    ~actual:(List.length (lines (Render.ascii Board.empty)))

(* ------------------------------------------------------------------ *)
(* Render: fancy                                                       *)
(* ------------------------------------------------------------------ *)

let reset = "\027[0m"
let bg n = Printf.sprintf "\027[48;5;%dm" n

(* The escape-free text of a fancy diagram of the starting position. *)
let fancy_initial_text =
  "8  \u{265C}  \u{265E}  \u{265D}  \u{265B}  \u{265A}  \u{265D}  \u{265E}  \u{265C} \n\
   7  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F} \n\
   6                         \n\
   5                         \n\
   4                         \n\
   3                         \n\
   2  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F}  \u{265F} \n\
   1  \u{265C}  \u{265E}  \u{265D}  \u{265B}  \u{265A}  \u{265D}  \u{265E}  \u{265C} \n\
  \   a  b  c  d  e  f  g  h \n"

(* [fancy_cells line] is the raw text of each square in a fancy rank line, in
   display order, escapes included. Squares are told apart by the background
   escape that starts each of them; the reset ending the line stays attached
   to the last square. *)
let fancy_cells line =
  match String.split_on_char '\027' line with
  | [] -> []
  | _label :: rest ->
      (* Each square starts with a background escape "[48;5;Nm"; a piece
         adds a foreground escape after it. Group them back up. *)
      let rec group acc = function
        | [] -> List.rev acc
        | part :: more when String.length part > 1 && String.sub part 0 2 = "[4"
          ->
            group (("\027" ^ part) :: acc) more
        | part :: more -> (
            match acc with
            | last :: acc' -> group ((last ^ "\027" ^ part) :: acc') more
            | [] -> group acc more)
      in
      group [] rest

let test_render_fancy () =
  let drawn = Render.ascii ~style:Render.Fancy Board.initial in
  check_string "fancy shows glyphs on the right squares"
    ~expected:fancy_initial_text ~actual:(strip_ansi drawn);
  check "every fancy line ends with a reset"
    (List.for_all
       (fun l -> String.ends_with ~suffix:reset l)
       (lines drawn));
  check "fancy uses color" (contains ~sub:"\027[48;5;" drawn);
  check_string "fancy from Black mirrors the board"
    ~expected:"1  \u{265C}  \u{265E}  \u{265D}  \u{265A}  \u{265B}  \u{265D}  \u{265E}  \u{265C} "
    ~actual:
      (List.hd
         (lines
            (strip_ansi
               (Render.ascii ~style:Render.Fancy ~perspective:Color.Black
                  Board.initial))));
  let rank1 = List.nth (lines drawn) 7 in
  let cells = fancy_cells rank1 in
  check_int "a rank has eight squares" ~expected:8 ~actual:(List.length cells);
  check "a1 is a dark square" (String.starts_with ~prefix:(bg 137) (List.nth cells 0));
  check "b1 is a light square"
    (String.starts_with ~prefix:(bg 180) (List.nth cells 1));
  check "white pieces are drawn in white"
    (contains ~sub:"\027[38;5;231m" (List.nth cells 0));
  let rank8 = fancy_cells (List.hd (lines drawn)) in
  check "black pieces are drawn in black"
    (contains ~sub:"\027[38;5;16m" (List.nth rank8 0))

let test_render_highlights () =
  (* e1 is file 4 of rank 1, the eighth line from the top for White. It is a
     dark square, like a1. *)
  let e1_cell hs =
    let drawn = Render.ascii ~style:Render.Fancy ~highlights:hs Board.initial in
    List.nth (fancy_cells (List.nth (lines drawn) 7)) 4
  in
  let starts_with n hs = String.starts_with ~prefix:(bg n) (e1_cell hs) in
  check "Last_move is yellow" (starts_with 186 [ (sq "e1", Render.Last_move) ]);
  check "Selected is blue" (starts_with 75 [ (sq "e1", Render.Selected) ]);
  check "Target is green" (starts_with 114 [ (sq "e1", Render.Target) ]);
  check "Check is red" (starts_with 203 [ (sq "e1", Render.Check) ]);
  check "Check beats everything"
    (starts_with 203
       [
         (sq "e1", Render.Last_move);
         (sq "e1", Render.Selected);
         (sq "e1", Render.Check);
         (sq "e1", Render.Target);
       ]);
  check "Selected beats Target and Last_move"
    (starts_with 75
       [
         (sq "e1", Render.Target);
         (sq "e1", Render.Selected);
         (sq "e1", Render.Last_move);
       ]);
  check "Target beats Last_move"
    (starts_with 114 [ (sq "e1", Render.Last_move); (sq "e1", Render.Target) ]);
  check "a highlight elsewhere leaves e1 alone"
    (starts_with 137 [ (sq "e2", Render.Check) ]);
  check_string "highlights do not change the visible text"
    ~expected:fancy_initial_text
    ~actual:
      (strip_ansi
         (Render.ascii ~style:Render.Fancy
            ~highlights:[ (sq "e1", Render.Check); (sq "e4", Render.Target) ]
            Board.initial))

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let run_guarded name f =
  try f () with
  | Failure m -> fail name ("raised Failure: " ^ m)
  | Invalid_argument m -> fail name ("raised Invalid_argument: " ^ m)

let () =
  print_endline "=== interface ===";
  List.iter
    (fun (name, f) ->
      print_endline name;
      run_guarded name f)
    [
      ("render: plain", test_render_plain);
      ("render: fancy", test_render_fancy);
      ("render: highlights", test_render_highlights);
    ];
  Printf.printf "\n%d interface checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
