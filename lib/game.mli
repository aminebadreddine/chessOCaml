(** A game: the current position together with everything that led to it.

    A {!Position} is enough to decide what may be played next. A game is what
    undo, the move list, threefold repetition, and save files need, and all
    four of those want the same thing — the moves played and the positions they
    produced — so the history is kept once, here, rather than three times in
    three modules.

    Like everything below it, a game is immutable: {!apply_unchecked} returns a
    new game and {!undo} returns the previous one. Undo therefore costs
    nothing; there is no un-move logic to write, because the old value was
    never destroyed.

    A game does not know its own {!Status.t}. Working that out needs the rules,
    which sit above this module; ask {!Rules.status} or {!Engine.status}. *)

type t

val create : unit -> t
(** [create ()] is a new game from the standard starting position. *)

val of_position : Position.t -> t
(** [of_position pos] is a game starting from [pos], with no history. Used when
    loading a saved position or setting up a test. *)

val position : t -> Position.t
(** [position g] is the current position. *)

val board : t -> Board.t
(** [board g] is the current board. Convenience for [Position.board (position
    g)]. *)

val turn : t -> Color.t
(** [turn g] is the player to move. *)

val ply : t -> int
(** [ply g] is how many moves have been played, 0 in a new game. *)

val history : t -> (Position.t * Move.t) list
(** [history g] pairs each past position with the move played in it, most
    recent first. The position in a pair is the one {b before} the move, so the
    first element of [history g] is everything needed to undo. *)

val moves : t -> Move.t list
(** [moves g] is the moves played, oldest first. This is the move list a user
    interface displays and a save file records. *)

val start_position : t -> Position.t
(** [start_position g] is the position the game began from. A save file needs
    this plus {!moves} and nothing else. *)

val apply_unchecked : t -> Move.t -> t
(** [apply_unchecked g move] plays [move] and records it. Legality is the
    caller's responsibility; see {!Position.apply_unchecked} and
    {!Engine.play}. *)

val undo : t -> t option
(** [undo g] is the game one move earlier, or [None] at the start. *)

val repetition_count : t -> int
(** [repetition_count g] is how many times the current position has occurred in
    this game, counting the present occurrence. Three means a draw may be
    claimed. Comparison is by {!Position.same_position}. *)

val is_fifty_move_draw : t -> bool
(** [is_fifty_move_draw g] is whether 100 plies have passed with no capture and
    no pawn move. *)
