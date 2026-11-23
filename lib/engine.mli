(** The checked front door to the engine. {b Shared; change by agreement.}

    Everything below this module is deliberately unchecked, because legality
    is defined in terms of playing moves. This module is the one place where
    the checks are applied, so the interface, the bot, and the save loader can
    all say "play this move" without each re-deriving what legal means.

    Rule of thumb for the team: if you are outside [lib], call {!Engine}. If
    you are implementing the rules, call the unchecked functions directly. *)

type error =
  | Illegal_move of Move.t
  | Game_already_over of Status.t
  | Nothing_to_undo

val error_to_string : error -> string
(** [error_to_string e] is a message fit to show a player. *)

val new_game : unit -> Game.t
(** [new_game ()] is a game at the starting position. *)

val play : Game.t -> Move.t -> (Game.t, error) result
(** [play g move] is the game after [move], or an error if the game is over or
    the move is not legal. This is the only move-playing function user-facing
    code should call.

    It generates the legal moves in order to check [move] against them, so it
    is the right function for a move typed by a player and the wrong one for a
    bot trying every move in turn — that would regenerate the list once per
    candidate. Code that already holds a move from {!legal_moves} knows it is
    legal and should use {!Game.apply_unchecked} directly. *)

val legal_moves : Game.t -> Move.t list
(** [legal_moves g] is every legal move in the current position. *)

val status : Game.t -> Status.t
(** [status g] is how the game stands. *)

val undo : Game.t -> (Game.t, error) result
(** [undo g] takes back the last move. *)
