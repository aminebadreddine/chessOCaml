(** Pseudo-legal move generation. {b Owner: Xavier. Backup: Amine.}

    This interface is a draft written so that the rest of the project
    compiles and so that everyone can code against it. The owner should change
    it freely — but announce changes in the group chat, because every module
    above depends on it.

    "Pseudo-legal" means: legal by the way the piece moves, ignoring whether
    the mover's own king ends up in check. A knight pinned against its king
    generates moves here; {!Rules} is what discards them. Splitting it this way
    is what keeps the dependency graph acyclic — deciding legality requires
    playing a move and asking whether the king is attacked, so legality cannot
    be the thing move generation is defined in terms of. *)

val pseudo_legal : Position.t -> Move.t list
(** [pseudo_legal pos] is every move the side to move could make by the
    movement rules of the pieces, including captures, castles, en passant, and
    the four promotion choices as four separate moves. Moves that leave the
    mover's own king in check are included. *)

val pseudo_legal_from : Position.t -> Square.t -> Move.t list
(** [pseudo_legal_from pos sq] is the subset of {!pseudo_legal} starting at
    [sq]. The interface needs this to highlight a selected piece's
    destinations; it is also much easier to debug one piece at a time. *)

val attacks : Position.t -> Color.t -> Square.t -> bool
(** [attacks pos color sq] is whether any piece belonging to [color] attacks
    [sq] in [pos], whether or not [sq] is occupied and whether or not moving
    there would be legal.

    This is the primitive check detection is built from: [color] gives check
    exactly when [attacks pos color (king square of the other color)].

    Two traps. A pawn attacks diagonally but does not attack the square it
    pushes to, so this cannot be written as "some pseudo-legal move lands
    here". And castling is not an attack, so it must be excluded or the two
    functions recurse forever. *)

val attackers : Position.t -> Color.t -> Square.t -> Square.t list
(** [attackers pos color sq] is every square from which [color] attacks [sq].
    The bot wants this for deciding whether a capture is defended; it is also
    the natural way to implement {!attacks}. *)
