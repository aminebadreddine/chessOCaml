(** Drawing the board. {b Owner: Amine. Backup: Yedidia.}

    Every function returns a string rather than printing. Keeping rendering
    pure means it can be unit-tested and cram-tested, and it is what the course
    architecture asks for: [lib] returns values, [bin] does the I/O.

    Render knows {b only} the board. It does not know whose turn it is, which
    moves are legal, or whether a king is in check; the caller works those out
    and passes them in as {!highlight}s. That keeps this module at the bottom
    of the dependency graph, so it can be used and tested before the rules
    exist. *)

type style =
  | Plain
      (** FEN letters and ['.'] for empty squares, two characters per square,
          no escape codes. Deterministic and readable in a failing test or a
          terminal without color. *)
  | Fancy
      (** Unicode chess glyphs on light and dark squares drawn with ANSI
          colors, three characters per square. For actual play. *)

(** Why a square is drawn differently from its neighbours. *)
type highlight =
  | Last_move  (** origin or destination of the previous move *)
  | Check  (** the square of a king in check *)
  | Selected  (** the square the player asked about *)
  | Target  (** a square the selected piece can move to *)

val ascii :
  ?style:style ->
  ?perspective:Color.t ->
  ?highlights:(Square.t * highlight) list ->
  Board.t ->
  string
(** [ascii ?style ?perspective ?highlights b] is a terminal diagram of [b]:
    eight rank lines, each starting with its rank number, then a line of file
    letters. Every line ends in ['\n'].

    - [style] defaults to [Plain]. With the defaults, the output is exactly
      {!Board.to_ascii}[ b].
    - [perspective] is the player sitting at the bottom of the diagram, and
      defaults to [White]: rank 8 at the top, files a to h. From [Black], rank
      1 is at the top and the files run h to a.
    - [highlights] marks squares. In [Fancy] style each kind has its own
      background color; if a square is listed more than once, [Check] beats
      [Selected], which beats [Target], which beats [Last_move]. [Plain] style
      has no color, so it shows only [Target], as ['*'] on an empty square,
      and ignores the rest.

    In [Fancy] style every line ends with the ANSI reset sequence before the
    newline, so color never leaks past the diagram. *)

val svg : ?last_move:Move.t -> Board.t -> string
(** [svg ?last_move b] is a complete SVG document drawing [b], highlighting
    [last_move] if given. Same shape as the A1 generator: build the text, let
    [bin] write the file. Not implemented yet. *)
