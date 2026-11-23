(** Computer opponent. {b Owner: Jah. Backup: Shehroze.}

    Deliberately not a search engine. [choose] does no lookahead; it picks
    from the legal moves by a few rules — prefer winning material,
    avoid leaving pieces where the opponent can take them — which is
    tractable in the time available and is enough to make the game playable
    alone.

    Every strategy scores each legal move and plays a best-scoring one. Scores
    are material swings measured with {!evaluate}, so captures, promotions,
    and en passant are all valued by the same rule without special cases. *)

type strategy =
  | Random  (** any legal move, uniformly *)
  | Greedy_capture
      (** the move that wins the most material right now, ignoring the reply *)
  | Avoid_threats
      (** as [Greedy_capture], but a move is charged the value of the most
          valuable piece it leaves capturable — any of the bot's pieces, not
          only the one that moved, so an attacked piece gets moved to safety.
          "Capturable" means the opponent has a legal capture there, so a
          piece pinned to its king is not counted as a threat. *)

val choose :
  ?strategy:strategy -> ?rng:Random.State.t -> Game.t -> Move.t option
(** [choose ?strategy ?rng g] is the bot's move, or [None] if there are no
    legal moves. The move is always one of {!Engine.legal_moves}[ g].
    [strategy] defaults to [Avoid_threats].

    Ties are broken at random using [rng], so the bot does not play the same
    game every time. Tests that need a reproducible move pass a seeded [rng]
    such as [Random.State.make [| 0 |]]; when omitted, a self-initialised
    state is used. *)

val piece_value : Piece.kind -> int
(** [piece_value k] is [k]'s worth in centipawns: pawn 100, knight 320,
    bishop 330, rook 500, queen 900. The king is 0 because it is never
    traded. *)

val evaluate : Position.t -> int
(** [evaluate pos] scores [pos] from White's point of view in centipawns:
    positive favours White. Material only. *)
