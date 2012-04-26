module Game.UIHandler where

import Control.Concurrent.Chan

import qualified Graphics.UI.SDL as SDL
import qualified Game.Event as Event
import qualified Game.UIEvent as UIEvent

handle_events :: (Chan Event.Event) -> IO ()
handle_events c = do
    e <- SDL.waitEvent
    case e of
        SDL.MouseMotion x y _ _ -> do
            let x' = fromIntegral x
            let y' = fromIntegral y
            writeChan c (Event.Ui (UIEvent.MouseMotion x' y'))
            handle_events c
        SDL.MouseButtonUp x y b -> do
            let x' = fromIntegral x
            let y' = fromIntegral y
            writeChan c (Event.Ui (UIEvent.MouseUp x' y' b))
            handle_events c
        SDL.MouseButtonDown x y b -> do
            let x' = fromIntegral x
            let y' = fromIntegral y
            writeChan c (Event.Ui (UIEvent.MouseDown x' y' b))
            handle_events c
        SDL.KeyDown (SDL.Keysym k _ _) -> do
            writeChan c (Event.Ui (UIEvent.KeyDown k))
            handle_events c
        SDL.KeyUp (SDL.Keysym k _ _) -> do
            writeChan c (Event.Ui (UIEvent.KeyUp k))
            handle_events c
        SDL.Quit -> return ()
        _ -> handle_events c
