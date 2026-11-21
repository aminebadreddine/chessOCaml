(** Saving and loading games. {b Owner: Amine. Backup: Yedidia.}

    Draft interface; the owner should change it and tell the group.

    A game is fully described by the position it started from and the moves
    played, so the format can be two lines: a FEN string, then the moves in
    long algebraic notation separated by spaces. Loading replays them. That is
    both smaller and easier to debug than serialising the board, and it
    preserves the history, which means undo still works after a load.

    These two functions are the only ones in [lib] that touch the filesystem,
    mirroring the A1 architecture where only the generator does I/O. *)

val to_string : Game.t -> string
(** [to_string g] serialises [g]. *)

val of_string : string -> (Game.t, string) result
(** [of_string s] rebuilds a game, replaying the moves through {!Engine.play}
    so that a corrupt file is rejected rather than producing an impossible
    game. *)

val save : string -> Game.t -> (unit, string) result
(** [save path g] writes [g] to [path]. *)

val load : string -> (Game.t, string) result
(** [load path] reads a game from [path]. *)
