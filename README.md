# Haskell-Sudoku

- cabal install split

## GHC

### Install
- $ sudo apt-get install haskell-platform

### Compile
- $ ghc -o sudoku sudoku.hs

### Run
- $ echo ".......12.5.4............3.7..6..4....1..........8....92....8.....51.7.......3..." | ./sudoku

Run multiple puzzles (i.e. 3) from txt
- $ head -n3 puzzles.txt | ./sudoku
