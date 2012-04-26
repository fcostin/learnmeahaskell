import Control.Concurrent

import qualified Graphics.UI.SDL as SDL

import qualified Game.Event as Event
import qualified Game.UIEvent as UIEvent
import qualified Game.UIHandler as UIHandler
import qualified Game.ViewEvent as ViewEvent
import qualified Game.WorldView as View
import qualified Game.Board as Game


data GameTurn = PlayerTurn | ComputerTurn

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]

    event_chan <- newChan
    view_event_chan <- newChan
    writeChan view_event_chan (ViewEvent.Redraw)

    forkIO (View.run view_event_chan)
    forkIO (UIHandler.handle_events event_chan)

    handle_events event_chan view_event_chan PlayerTurn


quit view_event_chan = do
    writeChan view_event_chan ViewEvent.End
    return ()


handle_events event_chan view_event_chan t = do
    e <- readChan event_chan
    case e of
        Event.Ui UIEvent.Quit -> quit view_event_chan
        Event.Ui (UIEvent.KeyDown SDL.SDLK_ESCAPE) -> quit view_event_chan
        Event.Ui (UIEvent.MouseMotion x y) -> do
            writeChan view_event_chan (ViewEvent.PositionCursor x y)
            writeChan view_event_chan (ViewEvent.Redraw)
            handle_events event_chan view_event_chan t
        Event.Ui (UIEvent.MouseUp x y SDL.ButtonLeft) -> do
            case t of
                PlayerTurn -> do
                    -- figure out tile coords
                    let i = (x `quot` 50) + 1
                    let j = (y `quot` 50) + 1
                    writeChan view_event_chan (ViewEvent.UpdateMove (Game.X, i, j))
                    writeChan view_event_chan (ViewEvent.Redraw)
                    -- launch thread to compute computer move
                    handle_events event_chan view_event_chan ComputerTurn
                ComputerTurn -> handle_events event_chan view_event_chan t
        _ -> handle_events event_chan view_event_chan t
