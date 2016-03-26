#Minesweaper

This is a minesweeper clone that runs in the terminal, coded in haskell.

###Setup

Clone the directory and run:

    cabal sanbox init
    cabal install --only-dependencies
    cabal run width height mines

Where width and height are the size of the grid, and mines is the number of mines.
