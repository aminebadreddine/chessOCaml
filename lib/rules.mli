(** Legality and game outcome. {b Owner: Shehroze. Backup: Jah.}

    Draft interface; the owner should change it and tell the group.

    This module is where "pseudo-legal" becomes "legal". The standard way, and
    the right one for this project, is generate-then-filter: take each
    pseudo-legal move, play it with {!Position.apply_unchecked}, and discard it
    if the mover's king is then attacked. That is slower than tracking pins
    directly and it is obviously correct, which is the trade a four-week
    project should make. *)

val is_in_check : Position.t -> Color.t -> bool
(** [is_in_check pos color] is whether [color]'s king is attacked in [pos]. *)

val is_legal : Position.t -> Move.t -> bool
(** [is_legal pos move] is whether [move] is pseudo-legal in [pos] and does not
    leave the mover's own king in check. *)

val legal_moves : Position.t -> Move.t list
(** [legal_moves pos] is every legal move for the side to move. An empty list
    means the game is over: checkmate if the player is in check, stalemate
    otherwise. *)

val legal_moves_from : Position.t -> Square.t -> Move.t list
(** [legal_moves_from pos sq] is the legal moves starting at [sq]. *)

val status : Game.t -> Status.t
(** [status g] is how [g] stands. Takes a game rather than a position because
    threefold repetition and the fifty-move rule need the history.

    Order matters: checkmate and stalemate are decided from [legal_moves]
    first, because a position with no legal moves is over whatever the clocks
    say. *)

val is_insufficient_material : Position.t -> bool
(** [is_insufficient_material pos] is whether neither side can possibly mate:
    king against king, king and bishop, or king and knight. Optional for V1. *)
