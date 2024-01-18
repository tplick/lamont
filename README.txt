README for Lamont v2.0 (January 2024)

SUMMARY

Lamont is an experimental solver for double-dummy problems in the game
of contract bridge.  An example run looks like the following.  (Some of
the output has been omitted for brevity.)

    #315
    -> West:  ♠A ♠8 ♠3 ♡A ♡Q ♡8 ♡2 ♣10 ♣3 ♢J ♢6 ♢4 ♢2 
       North:  ♠Q ♠7 ♠5 ♡10 ♡5 ♣J ♣8 ♣5 ♣4 ♢K ♢10 ♢9 ♢7 
       East:  ♠K ♠10 ♠6 ♡K ♡3 ♣A ♣Q ♣7 ♣2 ♢A ♢8 ♢5 ♢3 
       South:  ♠J ♠9 ♠4 ♠2 ♡J ♡9 ♡7 ♡6 ♡4 ♣K ♣9 ♣6 ♢Q 
    depth 52: value 3, cumul nodes 144318108, rec table has 119154 entries

The value 3 here means that with perfect play, the East-West partnership
will win 3 more tricks than the North-South partnership.  That is,
East-West will win 8 tricks, and North-South will win 5 tricks.  (There
are 13 tricks in total.)

Before I explain how to compile and run the code, let me give an overview
of some of the more interesting aspects of the program.

  - Aside from making the solver return the correct answer, my main concern
    so far has been to make sure that the solver can run fast enough.
    Once I reach my speed goal, I will add the features that will make the
    program more usable and user-friendly.

  - Only notrump play is implemented so far.

  - A 64-bit OCaml installation is required.  32-bit is not supported.

  - For now, only 64-bit ARM and x64 machines are supported.  The hand
    canonicalization step has been implemented using NEON intrinsics
    on 64-bit ARM platforms.  On x64 machines, this work is done using the
    extremely fast PEXT instruction.  If you are trying to compile the program
    on a platform other than 64-bit ARM or x64, please check out commit
    f1935acf and compile that code.  That is the last version before I added
    architecture-specific implementations.

  - The main core of the program is an alpha-beta pruning algorithm with
    several algorithmic optimizations:
    * A recommendation table is maintained.  This table stores the best move
      for each hand (with some bucketing performed) and is consulted the next
      time the hand is seen.
    * Since the performance of alpha-beta pruning is highly sensitive to move
      ordering, I spent a great deal of time experimenting to determine the
      best order in which to examine the cards in the hand.
    * It is often possible to tell when a player or side can win several tricks
      in a row; for example, if the current player holds all four aces in a
      notrump hand, he can play the aces in succession to win four tricks.
      A rapid search is carried out to catch situations like this, both the
      obvious cases and the not-so-obvious cases.
    * The hand canonicalization step (mentioned previously) increases the
      effectiveness of the transposition table.  It happens frequently that
      the table does not contain the exact hand for which we are searching, but
      it does contain a hand with an equivalent distribution of the cards.

  - Some micro-optimizations have been made as well.
    * Hands are stored as 52-bit integers.  This representation speeds up
      searching for particular cards in the hands, as well as common operations
      like playing the lowest or highest card in a suit.
    * Accesses to the transposition tables are guarded by checks to a Bloom filter.
      This gave a modest speedup to the table lookup code.
    * Calls to min, max, and comparisons have been specialized for their types
      (mostly integers), eliminating the overhead of the polymorphic versions.


HOW TO COMPILE

  - Install and set up opam using the directions at https://ocaml.org/install .

  - Using opam, install OCaml version 5.1.1.  The command to do this is

    opam switch create 5.1.1

    Make sure that this new OCaml installation is active by running
    `opam switch 5.1.1` and then `eval $(opam env)` .

  - Type `make` to generate the executable named exe.opt .


HOW TO RUN

  - As a first step, run

    ./exe.opt -quick-test

    The solver will run 20 preselected hands and check that the results are correct.
    If you see the program report an incorrect result, please reach out to me!

    (If you receive a message about a missing library, try adding LD_LIBRARY_PATH=. to
    the beginning of your command line.  I have seen this error on Linux but not on macOS.)

  - If the quick test runs successfully, you are ready to run the program on more hands.
    The program contains a database of 10,000 pregenerated hands.  (These are compiled in
    to the program but were generated at random using the RNG from a previous version of
    OCaml.)  Run

    ./exe.opt -bench 100

    to run the solver on the first 100 hands.  To solve a single hand, you can run

    ./exe.opt -single 1

    where the integer argument selects the hand.

  - Some hands are more difficult to solve than others.  Most hands will finish before
    a million nodes are examined, and a large number only require in the range of
    10,000 to 100,000 nodes to be examined.  Hands #315 and #8719 are the most stubborn.
    In version 2.0 of the solver, they require 144,318,108 nodes and 255,847,446 nodes
    respectively.  You can see this by running

    ./exe.opt -single 315
    ./exe.opt -single 8719

  - After making changes to the code, I re-run the program to check that the same solutions
    have been found.  To automate this somewhat, I have provided the script summarize.py
    into which you can pipe the output of the main program.  This script looks for the lines
    of the form

    #315: [1 2 3 4 5 6 7 6 5 6 5 4 3]

    that contain the result of the search iterations.  It gathers these lines and generates
    their md5 hash.  As an example, running

    ./exe.opt -bench 100 | ./summarize.py

    will print the output of the program followed by the hash 89094fca2afdcbbf0e18ec3136955a33.

    For reference, you should see these hashes:

    ./exe.opt -bench 100 | ./summarize.py         89094fca2afdcbbf0e18ec3136955a33

    ./exe.opt -bench 1000 | ./summarize.py        722dd84b4a550406e2691540171b481a

    ./exe.opt -bench 10000 | ./summarize.py       4b306b23ab63fb15412f831c9d8bb846

