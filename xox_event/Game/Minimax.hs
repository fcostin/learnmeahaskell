module Game.Minimax (minimax) where

import qualified Data.Map
import Game.Board (Outcome, Piece, Piece(X), Piece(O), Outcome(WinX), Outcome(Draw), Outcome(WinO))

type Tile = (Int, Int)

type Board = Data.Map.Map Tile Piece

-- [detect terminal game states]

invert :: Piece -> Piece
invert X = O
invert O = X

has_three_in_a_line :: Piece -> [Tile] -> Board -> Bool
has_three_in_a_line p ts b = and $ map (== p) $ map (\t -> Data.Map.findWithDefault (invert p) t b) ts

has_won :: Piece -> Board -> Bool
has_won p b = or $ map (\line -> has_three_in_a_line p line b) board_lines

board_full :: Board -> Bool
board_full b = and $ map (\t -> Data.Map.member t b) board_cells

evaluate_board :: Board -> Maybe Outcome
evaluate_board b = if has_won X b then Just WinX else
    if has_won O b then Just WinO else
    if board_full b then Just Draw else Nothing

board_cells :: [Tile]
board_cells = [(1,1), (1,2), (1,3),
    (2,1), (2,2), (2,3),
    (3,1), (3,2), (3,3)]

board_lines :: [[Tile]]
board_lines = [
    [(1,1),(1,2),(1,3)],
    [(2,1),(2,2),(2,3)],
    [(3,1),(3,2),(3,3)],
    [(1,1),(2,1),(3,1)],
    [(1,2),(2,2),(3,2)],
    [(1,3),(2,3),(3,3)],
    [(1,1),(2,2),(3,3)],
    [(1,3),(2,2),(3,1)]
    ]

-- [ai routines]

best_outcome :: Piece -> [(Outcome, Maybe Tile)] -> (Outcome, Maybe Tile)
best_outcome p ocs = case p of
    X -> minimum ocs
    O -> maximum ocs

free_cells :: Board -> [Tile]
free_cells b = filter (\x -> not $ Data.Map.member x b) board_cells

apply_move :: Tile -> Piece -> Board -> Board
apply_move t p b = Data.Map.insert t p b

-- expand returns list of pair of (successor game board, tile of the move made)
expand_moves :: Piece -> Board -> [(Board, Tile)]
expand_moves p b = map (\t -> (apply_move t p b, t)) $ free_cells b

minimax :: Piece -> Board -> (Outcome, Maybe Tile)
minimax p b = case evaluate_board b of
    Just o -> (o, Nothing)
    Nothing -> best_outcome p $ map (\bt -> (fst $ minimax (invert p) (fst bt), Just $ snd bt)) $ expand_moves p b
