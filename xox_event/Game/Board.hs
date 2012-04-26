module Game.Board where

data Piece = X | O
    deriving (Eq, Ord)

type Move = (Piece, Int, Int)

data Outcome = WinX | Draw | WinO
    deriving (Eq, Ord)
