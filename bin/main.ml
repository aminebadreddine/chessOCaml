(** Terminal entry point.

    This file reads arguments, prints, and chooses an exit status. Everything
    else belongs in [lib]: see ARCHITECTURE.md. The real game loop is the
    interface owner's; this is a smoke test that prints the starting position
    so that the engine can be exercised before there is a loop to run it in. *)

open ChessOCaml

let () =
  let game = Game.create () in
  print_string (Render.ascii (Game.board game));
  print_endline (Fen.of_position (Game.position game))
