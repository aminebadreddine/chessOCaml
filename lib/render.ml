(** Drawing the board. Owner: Amine. Backup: Yedidia. *)

type style =
  | Plain
  | Fancy

type highlight =
  | Last_move
  | Check
  | Selected
  | Target

(* ------------------------------------------------------------------ *)
(* Layout: shared by every style                                       *)
(* ------------------------------------------------------------------ *)

(* Ranks top to bottom and files left to right, as seen by [perspective]. *)
let ranks_top_down = function
  | Color.White -> [ 7; 6; 5; 4; 3; 2; 1; 0 ]
  | Color.Black -> [ 0; 1; 2; 3; 4; 5; 6; 7 ]

let files_left_right = function
  | Color.White -> [ 0; 1; 2; 3; 4; 5; 6; 7 ]
  | Color.Black -> [ 7; 6; 5; 4; 3; 2; 1; 0 ]

let square ~file ~rank =
  match Square.make ~file ~rank with
  | Some sq -> sq
  | None -> invalid_arg "Render: coordinates outside the board"

let file_letter file = String.make 1 (Char.chr (Char.code 'a' + file))

(* The highlights listed for [sq], in the order they were given. *)
let highlights_on highlights sq =
  List.filter_map
    (fun (s, h) -> if Square.equal s sq then Some h else None)
    highlights

(** How one style draws the pieces of a diagram. [draw] handles the layout, so
    a style only decides what a single square, rank label and file label look
    like. *)
type painter = {
  cell : Square.t -> Piece.t option -> highlight list -> string;
      (** a square, given its piece and every highlight on it *)
  rank_label : int -> string;
  file_label : int -> string;
  footer_prefix : string;  (** what precedes the file labels *)
  line_end : string;
}

let draw painter perspective highlights board =
  let buf = Buffer.create 1024 in
  let files = files_left_right perspective in
  List.iter
    (fun rank ->
      Buffer.add_string buf (painter.rank_label rank);
      List.iter
        (fun file ->
          let sq = square ~file ~rank in
          Buffer.add_string buf
            (painter.cell sq (Board.get board sq)
               (highlights_on highlights sq)))
        files;
      Buffer.add_string buf painter.line_end)
    (ranks_top_down perspective);
  Buffer.add_string buf painter.footer_prefix;
  List.iter (fun file -> Buffer.add_string buf (painter.file_label file)) files;
  Buffer.add_string buf painter.line_end;
  Buffer.contents buf

(* ------------------------------------------------------------------ *)
(* Plain style                                                          *)
(* ------------------------------------------------------------------ *)

(* Matches [Board.to_ascii] character for character: every square is followed
   by a space, and the file labels are preceded by one. *)
let plain =
  {
    cell =
      (fun _sq piece hs ->
        let c =
          match piece with
          | Some p -> Piece.to_char p
          | None -> if List.mem Target hs then '*' else '.'
        in
        String.make 1 c ^ " ");
    rank_label = (fun rank -> string_of_int (rank + 1) ^ " ");
    file_label = (fun file -> " " ^ file_letter file);
    footer_prefix = " ";
    line_end = "\n";
  }

(* ------------------------------------------------------------------ *)
(* Fancy style                                                          *)
(* ------------------------------------------------------------------ *)

(* Both sides use the filled glyphs: the outline ones are hard to see on a
   dark square. The side is told apart by the glyph's color instead. *)
let glyph = function
  | Piece.King -> "\u{265A}"
  | Piece.Queen -> "\u{265B}"
  | Piece.Rook -> "\u{265C}"
  | Piece.Bishop -> "\u{265D}"
  | Piece.Knight -> "\u{265E}"
  | Piece.Pawn -> "\u{265F}"

(* Colors from the 256-color ANSI palette. *)
let light_square = 180
let dark_square = 137
let white_piece = 231
let black_piece = 16

let highlight_color = function
  | Last_move -> 186 (* yellow *)
  | Selected -> 75 (* blue *)
  | Target -> 114 (* green *)
  | Check -> 203 (* red *)

let priority = function
  | Check -> 3
  | Selected -> 2
  | Target -> 1
  | Last_move -> 0

(* The highlight that wins on a square listed more than once. *)
let strongest hs =
  List.fold_left
    (fun best h ->
      match best with
      | Some b when priority b >= priority h -> best
      | _ -> Some h)
    None hs

let reset = "\027[0m"
let background n = Printf.sprintf "\027[48;5;%dm" n
let foreground n = Printf.sprintf "\027[38;5;%dm" n

(* a1 is a dark square, so a square is dark when file + rank is even. *)
let fancy_cell sq piece hs =
  let base =
    if (Square.file sq + Square.rank sq) mod 2 = 0 then dark_square
    else light_square
  in
  let bg =
    match strongest hs with Some h -> highlight_color h | None -> base
  in
  match piece with
  | None -> background bg ^ "   "
  | Some p ->
      let fg =
        match p.Piece.color with
        | Color.White -> white_piece
        | Color.Black -> black_piece
      in
      background bg ^ foreground fg ^ " " ^ glyph p.Piece.kind ^ " "

let fancy =
  {
    cell = fancy_cell;
    rank_label = (fun rank -> string_of_int (rank + 1) ^ " ");
    file_label = (fun file -> " " ^ file_letter file ^ " ");
    footer_prefix = "  ";
    line_end = reset ^ "\n";
  }

(* ------------------------------------------------------------------ *)
(* Public                                                               *)
(* ------------------------------------------------------------------ *)

let ascii ?(style = Plain) ?(perspective = Color.White) ?(highlights = []) b =
  match style with
  | Plain -> draw plain perspective highlights b
  | Fancy -> draw fancy perspective highlights b

let svg ?last_move (_b : Board.t) : string =
  ignore last_move;
  failwith "Render.svg: not implemented yet"
