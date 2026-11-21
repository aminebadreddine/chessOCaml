(** Turning what a player types into a move, and moves into text.
    {b Owner: Amine. Backup: Yedidia.}

    Draft interface; the owner should change it and tell the group.

    Note the shape of {!parse}: it takes a position. It has to. ["Nf3"] names
    no origin square, and even ["e4"] is a different move depending on where
    the pawns are. The robust way to write this is to generate the legal moves
    first and then ask which of them the input could mean — then ambiguity and
    illegality fall out as ordinary cases instead of needing their own
    parsing logic. *)

type error =
  | Malformed of string  (** not move-shaped at all *)
  | No_such_move of string  (** well-formed, but not legal here *)
  | Ambiguous of string * Move.t list  (** matches more than one legal move *)

val error_to_string : error -> string

val parse : Position.t -> string -> (Move.t, error) result
(** [parse pos s] interprets [s] as a move in [pos]. At minimum it should
    accept long algebraic ["e2e4"] and ["e7e8q"]; standard algebraic ["Nf3"],
    ["exd5"], ["O-O"] is the stretch. *)

val to_san : Position.t -> Move.t -> string
(** [to_san pos move] is standard algebraic notation for [move], the format a
    move-history panel shows. Needs the position to know whether a capture is
    involved and whether the piece letter needs a disambiguating file or
    rank. *)
