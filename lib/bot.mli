(** Computer opponent. {b Owner: Jah. Backup: Shehroze.}

    Draft interface; the owner should change it and tell the group.

    Deliberately not a search engine. [choose] picks from the legal moves by a
    few rules — prefer a capture of a valuable piece, avoid moving onto a
    square the opponent attacks — which is tractable in the time available and
    is enough to make the game playable alone. *)

type strategy =
  | Random
  | Greedy_capture
  | Avoid_threats

val choose : ?strategy:strategy -> Game.t -> Move.t option
(** [choose ?strategy g] is the bot's move, or [None] if there are no legal
    moves. Must return a legal move: build it from {!Engine.legal_moves} rather
    than generating candidates itself.

    Ties need a rule. Taking the first match makes the bot deterministic and so
    easy to test, but it also makes it play the same game every time; decide
    which matters more and write the decision down here. *)

val evaluate : Position.t -> int
(** [evaluate pos] scores [pos] from White's point of view in centipawns:
    positive favours White. Material only is a defensible V1. *)
