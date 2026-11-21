# Architecture

Read this before writing code. It is short on purpose.

## The one rule

**Dependencies point downward only.** A module may use anything below it in
this list and nothing above it. If you find yourself needing something from
above, that is a sign the thing belongs lower down — say so in the group chat
rather than adding an upward edge, because a cycle in OCaml is a compile error,
not a style problem, and untangling one late costs a day.

```
                      bin/main.ml            terminal: args, printing, exit code
                           |
    +----------+-----------+-----------+-----------+
    |          |                       |           |
  Engine    Notation                  Bot       Persist     checked entry points
    |          |                       |           |
    +----------+-----------+-----------+-----------+
                           |
                         Rules          legality, check, checkmate, draws
                           |
                        Movegen         how pieces move; attack detection
                           |
                         Game           position + history; undo; repetition
                           |
                       Position         board + turn + rights + ep + clocks
                           |
                         Board          64 squares, nothing else
                           |
            Move     Castling     Status      Render
                           |
             Square      Piece      Color      plain values
```

## Why the layers fall this way

The awkward fact about chess is that **legality is defined in terms of playing
a move**: a move is legal when, after you play it, your own king is not
attacked. So whatever plays a move cannot be the thing that checks legality, or
the two call each other forever.

The split that resolves it:

- `Position.apply_unchecked` plays *any* move, legal or not. It is named that
  way so nobody calls it by accident.
- `Movegen` produces *pseudo-legal* moves: correct by the movement rules,
  ignoring check.
- `Rules` plays each pseudo-legal move and discards the ones leaving its own
  king attacked. That is where "legal" first exists.
- `Engine` is the checked front door. **Code outside `lib` calls `Engine`, not
  `Position`.**

This is slower than tracking pins directly, and obviously correct. For a
four-week project that is the right trade.

## What each module may know

| Module | Knows | Must not know |
|---|---|---|
| `Color`, `Piece`, `Square` | plain values | anything else |
| `Board` | where pieces sit | whose turn it is, how pieces move |
| `Move` | from, to, promotion | whether it is a capture or castle |
| `Position` | board + turn + rights + en passant + clocks | which moves are legal |
| `Game` | a position and every position before it | its own status |
| `Movegen` | how pieces move, what attacks what | check, legality |
| `Rules` | legality, check, mate, draws | rendering, input |
| `Engine` | all of the above | terminal, files |
| `Render`, `Notation`, `Persist`, `Bot` | the engine's public interface | each other |

Two consequences worth stating outright:

- **`Move` carries no capture or castle flag.** A player typing `e2e4`, a
  parser, and the bot all build moves without consulting the board; what the
  move *does* is worked out by `Position.apply_unchecked` from the position.
  One move type for the whole team, instead of a variant only `Movegen` could
  construct.
- **`Game` does not know its own `Status`.** Working that out needs `Rules`,
  which is above it. Ask `Engine.status`.

## Conventions

- **Everything is immutable.** Every function returns a new value; no function
  mutates its argument. `Board` is an array internally and copies on every
  write — that invariant is stated in `board.ml` and must not be broken, or two
  "different" positions will share storage and the history will silently
  corrupt.
- **Undo is free.** The old value was never destroyed, so there is no un-move
  logic to write. Do not add any.
- **Every module in `lib` has a `.mli`.** The `.mli` is the contract; it is
  what everyone else reads. Write the OCamldoc comment before the code.
- **Nothing in `lib` prints or exits.** Library code returns values or
  `result`. Only `bin/main.ml` does I/O, with the single exception of
  `Persist`, which reads and writes save files. Same split as A1.
- **If you change an `.mli` someone else uses, say so in the group chat
  immediately.** That is the team contract, and it is the only coordination
  cost this design has.

## Adding a feature

The special rules are staged so that each one is a small function nobody else
has to know about. En passant, promotion, and castling are already carved out
as five helpers at the bottom of `position.ml`, each currently a no-op that
returns its input unchanged. That is why the engine already plays ordinary
chess: implementing a feature means filling in one helper, not editing
`apply_unchecked`.

Each has a failing test in the staged group of the test suite. Fill in the
helper, watch the test go green, move it into the foundation group.
