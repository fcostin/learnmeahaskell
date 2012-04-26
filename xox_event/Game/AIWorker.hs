module Game.AIWorker where

import Control.Concurrent
import qualified Data.Map as Map

import qualified Game.World as World
import qualified Game.Board as Game
import qualified Game.GameEvent as GameEvent
import qualified Game.Event as Event
import qualified Game.Minimax as Minimax

run :: Game.Piece -> World.World -> Chan Event.Event -> IO ()
run p w c = do
    let f (p', i, j) = ((i, j), p')
    let board = Map.fromList $ map f w
    case Minimax.minimax p board of
        (_, Just (i, j)) -> do
            writeChan c (Event.Game (GameEvent.MakeMove (p, i, j)))
            return ()
        _ -> return ()
