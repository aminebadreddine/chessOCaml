# chessOCaml

A chess engine in OCaml. CS 3110, Fall 2026.

Amine Badreddine (ab3234), Yedidia Alebachew (ya287), Shehroze Kiani (szk7), Xavier Blake-Forbes (xb62), Jahan Kulkarni (jk2959). Areas of responsibility are in [AUTHORS.md](AUTHORS.md).

```
dune build        # must stay clean on main
dune test         # runs the suite
dune exec chessOCaml   # prints the starting position
```

`dune exec chessOCaml` currently prints:

```
8 r n b q k b n r
7 p p p p p p p p
6 . . . . . . . .
5 . . . . . . . .
4 . . . . . . . .
3 . . . . . . . .
2 P P P P P P P P
1 R N B Q K B N R
  a b c d e f g h
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
```

Read [ARCHITECTURE.md](ARCHITECTURE.md) before writing code. The short version:
dependencies point downward only, everything is immutable, every module has an
`.mli`, and code outside `lib` calls `Engine` rather than `Position`.

## What works now

The foundation is complete and tested: squares, pieces, moves, castling
rights, the board, positions, game history with undo and repetition counting,
and FEN printing. `dune test` reports 103 checks with the foundation green.

Ordinary chess moves already work — pushes, piece moves, and captures update
the board, the turn, both clocks, and the move number correctly. What does not
work yet is the special rules and everything above `Position` in the module
graph.

## What is left, and whose it is

The test suite has two groups. **Foundation** must stay green; a red check
there means someone has broken something the rest of the team depends on.
**Staged** is the to-do list — those failures are expected and do not fail the
build. Move a test from staged to foundation as its feature lands.

| Owner | Module | State |
|---|---|---|
| Yedidia | `Board`, `Position`, `Game`, `Fen` | done except five helpers in `position.ml` and `Fen.to_position` |
| Xavier | `Movegen` | interface drafted, not implemented |
| Shehroze | `Rules` | interface drafted, not implemented |
| Amine | `Notation`, `Render`, `Persist` | interfaces drafted; `Render.ascii` works as a debug renderer |
| Jah | `Bot`, tests | interface drafted, not implemented |

The drafted interfaces exist so that everything compiles and nobody is blocked.
**They are drafts.** The owner should change their own `.mli` as they learn
what the module actually needs, and announce the change in the group chat.

## Remaining foundation work

Five one-rule helpers at the bottom of `lib/position.ml`, each currently
returning its input unchanged, each with a failing test waiting:

1. `next_en_passant_target` — set the target after a double pawn push
2. `capture_en_passant_victim` — remove the pawn that is not on the destination
3. `promote` — swap the pawn for the chosen piece
4. `move_castling_rook` — move the rook when the king jumps two files
5. `next_castling_rights` — withdraw rights when the king moves, the rook
   moves, **or the rook is captured**

Plus `Fen.to_position`, the parser. Its whole test is the round trip against
`Fen.of_position`, which already works.

## Testing

No external test library, so a fresh clone runs `dune test` with nothing
installed. If we move to OUnit2 later, only the harness at the top of
`test/test_chessOCaml.ml` changes.

Once `Movegen` exists, the cheapest way to know it is right is perft: count
the leaf nodes at depth 1 through 4 from the starting position and compare
against the published figures (20, 400, 8902, 197281). A move generator that
matches those is almost certainly correct, which is not something most student
projects can say about their core.
