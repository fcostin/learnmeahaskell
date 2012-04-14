-- ----------------------------------------------------------------------------
-- "THE BEER-WARE LICENSE" (Revision 42):
-- <reuben.fletchercostin@gmail.com> wrote this file. As long as you retain this
-- notice you can do whatever you want with this stuff. If we meet some day,
-- and you think this stuff is worth it, you can buy me a beer in return
-- ----------------------------------------------------------------------------

-- my very first not-utterly-trivial haskell program : noughts and crosses

import Data.List
import qualified Data.Map

data Outcome = WinX | Draw | WinO
    deriving (Eq, Ord)

data Piece = X | O
    deriving Eq

type Tile = (Int, Int)

type Board = Data.Map.Map Tile Piece

-- [input routines]

-- parse int from string. returns [(parsed int, rest of string)] or [] (?) ...
read_int :: String -> [(Int, String)]
read_int s = reads s

parse_coords :: String -> [Int]
parse_coords player_input = map fst $ concat $ map read_int $ words player_input

valid_coord :: Int -> Bool
valid_coord x = 1 <= x && x <= 3

make_tile_from_coords :: [Int] -> Maybe Tile
make_tile_from_coords (i:j:[]) | valid_coord(i) && valid_coord(j) = Just (i, j)
make_tile_from_coords _ = Nothing

-- update the board with a move entered by the player
-- keep polling for input until we get a legal one
apply_player_move board = do
    putStrLn "Enter your move:"
    player_input <- getLine
    let tile = make_tile_from_coords $ parse_coords player_input
    case tile of
        Just t -> case Data.Map.member t board of
            False -> return $ Data.Map.insert t X board
            True -> do
                putStrLn("Invalid move - that tile is already occupied")
                apply_player_move board
        Nothing -> do
            putStrLn("Cannot understand your move -- the input format is 'col row' where col and row are values from {1,2,3}")
            apply_player_move board

-- [board formatting routines]

fmt_board_cell :: Int -> Int -> Board -> String
fmt_board_cell i j b = case Data.Map.lookup (i, j) b of
    Just X -> "X"
    Just O -> "O"
    Nothing -> "_"

fmt_board_row :: Int -> Board -> String
fmt_board_row i b = concat $ intersperse "\t" $ map (\j -> fmt_board_cell i j b) [1,2,3]

fmt_board :: Board -> String
fmt_board b = (concat $ intersperse "\n" $ map (\i -> fmt_board_row i b) [1,2,3]) ++ "\n"

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

apply_move t p b = Data.Map.insert t p b

-- expand returns list of pair of (successor game board, tile of the move made)
expand_moves :: Piece -> Board -> [(Board, Tile)]
expand_moves p b = map (\t -> (apply_move t p b, t)) $ free_cells b

minimax :: Piece -> Board -> (Outcome, Maybe Tile)
minimax p b = case evaluate_board b of
    Just o -> (o, Nothing)
    Nothing -> best_outcome p $ map (\bt -> (fst $ minimax (invert p) (fst bt), Just $ snd bt)) $ expand_moves p b

-- [top level mutually recursive routines for making and evaluating moves]

evaluate_turn b p = do
    putStr $ fmt_board b
    case evaluate_board b of
        Just WinX -> putStr "You win? Inconceivable!"
        Just WinO -> putStr "I win."
        Just Draw -> putStr "We draw."
        Nothing -> process_turn b $ invert p

process_turn b p = do
    putStr $ fmt_board b
    case p of
        X -> do
            b' <- apply_player_move b
            evaluate_turn b' p
        O -> do
            case minimax O b of
                (_, Just move) -> do
                    putStrLn ("I move to " ++ (show move))
                    evaluate_turn (apply_move move O b) p
                (_, Nothing) -> do
                    putStrLn "I can make no legal move?!"
                    evaluate_turn b p

-- [entry point]

main = do
    putStrLn "Lets play a game of noughts and crosses. You're crosses. Go on, make the first move."
    let board = Data.Map.empty
    process_turn board X
