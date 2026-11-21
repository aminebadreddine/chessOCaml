(** Drawing the board. {b Owner: Amine. Backup: Yedidia.}

    Draft interface; the owner should change it and tell the group.

    Both functions return a string rather than printing. Keeping rendering
    pure means it can be unit-tested and cram-tested, and it is what the course
    architecture asks for: [lib] returns values, [bin] does the I/O. *)

val ascii : Board.t -> string
(** [ascii b] is a terminal diagram of [b]. See {!Board.to_ascii} for the
    debugging version this replaces. *)

val svg : ?last_move:Move.t -> Board.t -> string
(** [svg ?last_move b] is a complete SVG document drawing [b], highlighting
    [last_move] if given. Same shape as the A1 generator: build the text, let
    [bin] write the file. *)
