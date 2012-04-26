import Control.Concurrent

import qualified Graphics.UI.SDL as SDL

import qualified Game.Event as Event
import qualified Game.UIEvent as UIEvent
import qualified Game.GameEvent as GameEvent
import qualified Game.UIHandler as UIHandler
import qualified Game.ViewEvent as ViewEvent
import qualified Game.WorldView as View
import qualified Game.Board as Game
import qualified Game.World as World
import qualified Game.AIWorker as AIWorker


data GameTurn = PlayerTurn | ComputerTurn

next_turn :: GameTurn -> GameTurn
next_turn PlayerTurn = ComputerTurn
next_turn ComputerTurn = PlayerTurn

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]

    event_chan <- newChan
    view_event_chan <- newChan

    _ <- forkIO (View.run view_event_chan)
    _ <- forkIO (UIHandler.handle_events event_chan)

    let world = World.empty_world
    writeChan view_event_chan (ViewEvent.Redraw world)

    handle_events event_chan view_event_chan PlayerTurn world


handle_events :: Chan Event.Event -> Chan ViewEvent.ViewEvent -> GameTurn -> World.World -> IO ()
handle_events event_chan view_event_chan t w = do
    e <- readChan event_chan
    case e of
        Event.UI UIEvent.Quit -> return ()
        Event.UI (UIEvent.KeyDown SDL.SDLK_ESCAPE) -> return ()
        Event.UI (UIEvent.MouseMotion x y) -> do
            writeChan view_event_chan (ViewEvent.PositionCursor x y)
            writeChan view_event_chan (ViewEvent.Redraw w)
            handle_events event_chan view_event_chan t w
        Event.UI (UIEvent.MouseUp x y SDL.ButtonLeft) -> do
            case t of
                PlayerTurn -> do
                    -- figure out which tile the player clicked on (hack)
                    let i = (x `quot` 50) + 1
                    let j = (y `quot` 50) + 1
                    writeChan event_chan (Event.Game (GameEvent.MakeMove (Game.X, i, j)))
                    handle_events event_chan view_event_chan t w
                ComputerTurn -> handle_events event_chan view_event_chan t w
        Event.Game (GameEvent.MakeMove (p, i, j)) -> do
            let w' = World.apply_move (p, i, j) w
            case (w == w') of
                True -> handle_events event_chan view_event_chan t w
                False -> do
                    writeChan view_event_chan (ViewEvent.Redraw w')
                    let t' = (next_turn t)
                    case t of
                        PlayerTurn -> do
                            _ <- forkIO (AIWorker.run Game.O w' event_chan)
                            return ()
                        ComputerTurn -> return ()
                    handle_events event_chan view_event_chan t' w'
        _ -> handle_events event_chan view_event_chan t w

