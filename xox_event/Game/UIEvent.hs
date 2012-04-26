module Game.UIEvent where

import qualified Graphics.UI.SDL as SDL

data UIEvent =
    KeyDown SDL.SDLKey |
    KeyUp SDL.SDLKey |
    MouseMotion Int Int |
    MouseDown Int Int SDL.MouseButton |
    MouseUp Int Int SDL.MouseButton |
    Quit
