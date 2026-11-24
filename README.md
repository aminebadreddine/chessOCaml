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
and FEN both ways. `dune test` reports 232 checks, all green.

Every chess rule that is a property of a *single move* is implemented —
ordinary moves and captures, en passant, promotion, castling including the
rook's jump, castling rights lost by moving or by having a rook captured, the
en passant target, both clocks, and the move number. `Position.apply_unchecked`
plays any of them correctly.

Move generation, legality and the bot are in place too, so a full legal game
can be played through `Engine`: illegal moves are rejected, and check,
checkmate, stalemate and the draw rules are detected. What is missing is
castling and en passant generation in `Movegen`, and the terminal game loop
that lets a player type moves.

## What is left, and whose it is

| Owner | Module | State |
|---|---|---|
| Yedidia | `Board`, `Position`, `Game`, `Fen` | **done** |
| Xavier | `Movegen` | done except implementation for castling and en passant |
| Shehroze | `Rules` | **done** |
| Amine | `Notation`, `Render`, `Persist`, game loop | `Render.ascii` works; Fancy rendering in PR #2; `Notation`, `Persist` and the game loop not started |
| Jah | `Bot`, tests | **done**: three strategies, with tests |

The drafted interfaces exist so that everything compiles and nobody is blocked.
**They are drafts.** The owner should change their own `.mli` as they learn
what the module actually needs, and announce the change in the group chat.

`Fen.to_position` is the thing to reach for when writing tests: it builds any
position from one line of text, so a test case is a FEN string rather than a
dozen calls to `Board.set`.

## Verifying the engine

Once `Movegen` and `Rules` exist, the cheapest way to know they are right is
perft: count the leaf nodes of the move tree to a given depth and compare
against published figures. From the starting position the counts are 20, 400,
8902, and 197281 for depths 1 to 4.

That is a far stronger check than hand-written cases, because a single wrong
rule anywhere shows up as a wrong total. Use these five positions, which exist
precisely to catch the rules that are easy to get wrong:

| Position | FEN | Depths 1-3 |
|---|---|---|
| start | `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1` | 20, 400, 8902 |
| Kiwipete | `r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1` | 48, 2039, 97862 |
| en passant | `8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1` | 14, 191, 2812 |
| promotions | `r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1` | 6, 264, 9467 |
| position 5 | `rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8` | 44, 1486, 62379 |

`Fen.to_position` turns each row into a position in one line. The foundation
has already been checked against all five with a scratch move generator, so a
mismatch points at `Movegen` or `Rules`, not at the board or the position.

## Testing

No external test library, so a fresh clone runs `dune test` with nothing
installed. If we move to OUnit2 later, only the harness at the top of
`test/test_chessOCaml.ml` changes.

The suite is one file today. When it outgrows that, split it by area and
switch `test/dune` from `(test (name ...))` to `(tests (names test_board
test_movegen ...))`, which builds each as its own executable. Splitting early
is worth it once two people are adding tests, since one file is one merge
conflict.

Once `Movegen` exists, the cheapest way to know it is right is perft: count
the leaf nodes at depth 1 through 4 from the starting position and compare
against the published figures (20, 400, 8902, 197281). A move generator that
matches those is almost certainly correct, which is not something most student
projects can say about their core.
