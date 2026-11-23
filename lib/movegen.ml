(** Pseudo-legal move generation. Owner: Xavier. Backup: Amine. *)



(* ------------------------------------------------------------------ *)
(* Psuedo legal helpers                                               *)
(* ------------------------------------------------------------------ *)



(** Creates a square.t list representing all psuedo-reachable squares from this knight *)
let knight_move_helper (_pos : Position.t) (_sq : Square.t) : Square.t list = 
  (
    let (sq_list : Square.t  option list) = [] in 
    let rank = Square.rank _sq in 
    let file = Square.file _sq in 

    let sq_list =  Square.make ~file:(file -2) ~rank:(rank -1 ) :: Square.make ~file:(file -2) ~rank:(rank + 1) :: Square.make ~file:(file + 2) ~rank:(rank -1 ) :: Square.make ~file:(file + 2) ~rank:(rank + 1) :: Square.make ~file:(file - 1) ~rank:(rank - 2) :: Square.make ~file:(file -1) ~rank:(rank + 2) :: Square.make ~file:(file + 1) ~rank:(rank - 2) :: Square.make ~file:(file + 1) ~rank:(rank + 2) :: sq_list in 

    let result = List.filter (fun a -> match a with | None -> false | Some _ -> true) sq_list in 
    let result = List.map (fun a -> match a with | Some square -> square | _ -> assert false) result in  
    result
  )

(** Creates a square.t list representing all psuedo-reachable squares from this king*)
let king_move_helper (_pos : Position.t) (_sq : Square.t) : Square.t list = 
  (
    let (sq_list : Square.t  option list) = [] in 
    let rank = Square.rank _sq in 
    let file = Square.file _sq in 

    let sq_list =      Square.make ~file:(file -1) ~rank:(rank -1 ) :: Square.make ~file:(file -1) ~rank:(rank +1 ) :: Square.make ~file:(file +1) ~rank:(rank -1 ) :: Square.make ~file:(file + 1) ~rank:(rank +1  ) :: Square.make ~file:(file) ~rank:(rank +1 ) :: Square.make ~file:(file) ~rank:(rank -1 ) :: Square.make ~file:(file -1) ~rank:(rank) :: Square.make ~file:(file +1) ~rank:(rank) :: sq_list in 
    let result = List.filter (fun a -> match a with | None -> false | Some _ -> true) sq_list in 
    let result = List.map (fun a -> match a with | Some square -> square | _ -> assert false) result in 
    result
)

(**Creates a square.t list representing all psuedo-reachable squares from this pawn*)
let pawn_move_helper (_pos : Position.t) (_sq : Square.t) : Square.t list =
(
  let rank = Square.rank _sq in
  let file = Square.file _sq in

  let piece = Position.piece_at _pos _sq in

  match piece with
  | None -> assert false

  | Some pawn ->
      let color = pawn.color in

      match color with

      | Color.Black ->

          let forward =
            match Square.make ~file ~rank:(rank - 1) with
            | None -> []
            | Some sq ->
                match Position.piece_at _pos sq with
                | None -> [sq]
                | Some _ -> []
          in

          let double_forward =
            if rank = 6 then
              match Square.make ~file ~rank:(rank - 2) with
              | None -> []
              | Some sq ->
                  match forward with
                  | [] -> []
                  | _ ->
                      match Position.piece_at _pos sq with
                      | None -> [sq]
                      | Some _ -> []
            else []
          in

          (* Diagonal captures *)
          let diagonal_capture f =
            match Square.make ~file:f ~rank:(rank - 1) with
            | None -> []
            | Some sq ->
                match Position.piece_at _pos sq with
                | None -> []
                | Some p ->
                    if p.color <> color then [sq]
                    else []
          in

          forward @ double_forward @
          diagonal_capture (file - 1) @
          diagonal_capture (file + 1)

      | Color.White -> (
        let forward =
            match Square.make ~file ~rank:(rank + 1) with
            | None -> []
            | Some sq ->
                match Position.piece_at _pos sq with
                | None -> [sq]
                | Some _ -> [] in 
        let double_forward =
            if rank = 1 then
              match Square.make ~file ~rank:(rank + 2) with
              | None -> []
              | Some sq ->
                  match forward with
                  | [] -> []
                  | _ ->
                      match Position.piece_at _pos sq with
                      | None -> [sq]
                      | Some _ -> []
            else []
          in
            (* Diagonal captures *)
          let diagonal_capture f =
            match Square.make ~file:f ~rank:(rank + 1) with
            | None -> []
            | Some sq ->
                match Position.piece_at _pos sq with
                | None -> []
                | Some p ->
                    if p.color <> color then [sq]
                    else []
          in

          forward @ double_forward @
          diagonal_capture (file - 1) @
          diagonal_capture (file + 1)


      )
)


     
  let rec rook_direction_helper (_pos : Position.t) (_sq : Square.t) (direction : int * int) (squares : Square.t list) (count : int) : Square.t list =
  let rank = Square.rank _sq in
  let file = Square.file _sq in
  let next_file = file + (fst direction * count) in
  let next_rank = rank + (snd direction * count) in
  let square = Square.make ~file:next_file ~rank:next_rank in
  match square with
  | None -> squares
  | Some s ->
      match Position.piece_at _pos s with
      | None ->
          rook_direction_helper _pos _sq direction (s :: squares) (count + 1)
      | Some _ ->  s :: squares
              




(**Creates a square.t list representing all psuedo-reachable squares from this rook*)
let rook_move_helper (_pos : Position.t) (_sq : Square.t) : Square.t list = 

  rook_direction_helper _pos  _sq (0,1) [] 1 @ rook_direction_helper _pos  _sq (0,-1) [] 1 @ rook_direction_helper _pos  _sq (1,0) [] 1 @ rook_direction_helper _pos  _sq (-1,0) [] 1

(**Creates a square.t list representing all psuedo-reachable squares from this bishop*)
let bishop_move_helper (_pos : Position.t) (_sq : Square.t) : Square.t list = 

  rook_direction_helper _pos  _sq (1,1) [] 1 @ rook_direction_helper _pos  _sq (1,-1) [] 1 @ rook_direction_helper _pos  _sq (-1,-1) [] 1 @ rook_direction_helper _pos  _sq (-1,1) [] 1



(**Creates a square.t list representing all psuedo-reachable squares from this queen*)
let queen_move_helper (_pos : Position.t) (_sq : Square.t) : Square.t list =  
  rook_move_helper (_pos : Position.t) (_sq : Square.t) @ bishop_move_helper (_pos : Position.t) (_sq : Square.t) 



(**Returns the move promoting a pawn if it reaches the end of the board, otherwise returns the regular move*)
let promote (move : Move.t) = 
  
  let rank = Square.rank move.to_ in
  if (rank <> 7 && rank <> 0 ) then [move] else 
    Move.make ~promotion:Piece.Bishop move.from move.to_ :: Move.make ~promotion:Piece.Knight move.from move.to_ :: Move.make ~promotion:Piece.Queen move.from move.to_ :: [Move.make ~promotion:Piece.Rook move.from move.to_]


(* ------------------------------------------------------------------ *)
(* Psuedo legal functions                                             *)
(* ------------------------------------------------------------------ *)

let pseudo_legal_from (_pos : Position.t) (_sq : Square.t) : Move.t list =
  let piece_op = Position.piece_at _pos _sq in 
  let current_turn = Position.turn _pos in 
  match piece_op with 
  | None -> []
  | Some piece -> ( if current_turn <> piece.color then []  else 
     match piece.kind with 
     | Piece.Knight -> let squares = knight_move_helper _pos _sq in  let squares =  List.filter (fun a ->  match Position.piece_at _pos a  with | None -> true | Some piece -> piece.color <> Position.turn _pos ) squares in List.map (fun a -> Move.make _sq a) squares 
     | Piece.King -> let squares = king_move_helper _pos _sq in let squares = List.filter (fun a ->  match Position.piece_at _pos a  with | None -> true | Some piece -> piece.color <> Position.turn _pos ) squares in  List.map (fun a -> Move.make _sq a) squares 
     | Piece.Pawn -> let squares = pawn_move_helper _pos _sq in  let moves = List.map (fun a -> Move.make _sq a) squares in  List.concat (List.map promote moves)
     | Piece.Rook ->
      let squares = rook_move_helper _pos _sq in
      let squares =
      List.filter
        (fun a ->
          match Position.piece_at _pos a with
          | None -> true
          | Some piece -> piece.color <> Position.turn _pos)
        squares
      in
      List.map (fun a -> Move.make _sq a) squares 
     | Piece.Bishop -> let squares = bishop_move_helper _pos _sq in  let squares =
      List.filter
        (fun a ->
          match Position.piece_at _pos a with
          | None -> true
          | Some piece -> piece.color <> Position.turn _pos)
        squares
      in
      List.map (fun a -> Move.make _sq a) squares 
     | Piece.Queen -> let squares = queen_move_helper _pos _sq in  let squares =
      List.filter
        (fun a ->
          match Position.piece_at _pos a with
          | None -> true
          | Some piece -> piece.color <> Position.turn _pos)
        squares
      in
      List.map (fun a -> Move.make _sq a) squares 
     )

let pseudo_legal (_pos : Position.t) : Move.t list = 
  let turn = Position.turn _pos in let pieces = Board.pieces_of (Position.board _pos) turn in 
  List.concat (List.map (fun (s,_)-> pseudo_legal_from _pos s) pieces)



(* ------------------------------------------------------------------ *)
(* Attacking helpers                                                  *)
(* ------------------------------------------------------------------ *)


(**Returns the list of squares containing the knights attacking the given square*)
let knight_attackers_helper (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : Square.t list =
  let potential_knights = knight_move_helper _pos  _sq in 
  List.filter(fun a -> let op = Position.piece_at _pos a in match op with 
    | None -> false | Some piece -> piece.color = _color && piece.kind = Piece.Knight) potential_knights


let king_attackers_helper (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : Square.t list =
  let potential_kings = king_move_helper _pos  _sq in 
  List.filter(fun a -> let op = Position.piece_at _pos a in match op with 
    | None -> false | Some piece -> piece.color = _color && piece.kind = Piece.King) potential_kings


let rook_attackers_helper (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : Square.t list =
  let potential_rooks = rook_move_helper _pos  _sq in 
  List.filter(fun a -> let op = Position.piece_at _pos a in match op with 
    | None -> false | Some piece -> piece.color = _color && piece.kind = Piece.Rook) potential_rooks

let bishop_attackers_helper (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : Square.t list =
  let potential_bishops = bishop_move_helper _pos  _sq in 
  List.filter(fun a -> let op = Position.piece_at _pos a in match op with 
    | None -> false | Some piece -> piece.color = _color && piece.kind = Piece.Bishop) potential_bishops

let queen_attackers_helper (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : Square.t list =
  let potential_queens = queen_move_helper _pos  _sq in 
  List.filter(fun a -> let op = Position.piece_at _pos a in match op with 
    | None -> false | Some piece -> piece.color = _color && piece.kind = Piece.Queen) potential_queens


let pawn_attackers_helper (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : Square.t list =
  let rank = Square.rank _sq in
  let file = Square.file _sq in 

  match _color with 
  | Color.Black -> 
    let potential_pawns = Square.make ~file:(file+1) ~rank:(rank+1) :: [Square.make ~file:(file-1) ~rank:(rank+1)] in 
    List.filter(fun a -> let op = Position.piece_at _pos a in match op with 
    | None -> false | Some piece -> piece.color = _color && piece.kind = Piece.Pawn) (List.map (fun b -> match b with | Some c -> c | None -> assert false)(List.filter (fun a-> match a with |None -> false |Some _ -> true) potential_pawns))
  
  | Color.White -> (
    let potential_pawns = Square.make ~file:(file+1) ~rank:(rank-1) :: [Square.make ~file:(file-1) ~rank:(rank-1)] in 
    List.filter(fun a -> let op = Position.piece_at _pos a in match op with 
    | None -> false | Some piece -> piece.color = _color && piece.kind = Piece.Pawn) (List.map (fun b -> match b with | Some c -> c | None -> assert false)(List.filter (fun a-> match a with |None -> false |Some _ -> true) potential_pawns))
  )
(* ------------------------------------------------------------------ *)
(* Attacking functions                                                *)
(* ------------------------------------------------------------------ *)
  

let attackers (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : Square.t list = 
    (knight_attackers_helper _pos _color _sq) @ (king_attackers_helper _pos _color _sq)
    @ (bishop_attackers_helper _pos _color _sq) @ (rook_attackers_helper _pos _color _sq)
    @ (pawn_attackers_helper _pos _color _sq) @ (queen_attackers_helper _pos _color _sq)

let attacks (_pos : Position.t) (_color : Color.t) (_sq : Square.t) : bool =
  attackers _pos _color _sq <> []