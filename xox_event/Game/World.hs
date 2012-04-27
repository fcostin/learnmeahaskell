module Game.World where

import qualified Data.List as List
import qualified Game.Board as Game

type World = [Game.Move]

empty_world :: World
empty_world = []

apply_move :: Game.Move -> World -> World
apply_move (p, i, j) w =
    case List.find (\ (_, i', j') -> (i == i') && (j == j')) w of
        Just _ -> w -- ignore duplicate moves
        Nothing -> ((p, i, j):w)

