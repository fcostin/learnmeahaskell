module Game.WorldView where

import Control.Concurrent.Chan

import Graphics.UI.SDL as SDL
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Game.ViewEvent as ViewEvent
import qualified Game.Board as Game

screen_width = 150
screen_height = 150
screen_depth = 32
screen_flags = [HWSurface, DoubleBuf]
screen_bg_colour_rgb = 0x000000
screen_cursor_colour_rgb = 0xFFFFFF
screen_cursor_size = 10
sprite_width = 50
sprite_height = 50
transparent_sprite_colour_rgb = 0xFF00FF

type World = [Game.Move]

type Sprites = Map.Map Game.Piece Surface

type Cursor = (Int, Int)

empty_world :: World
empty_world = []

run :: (Chan ViewEvent.ViewEvent) -> IO ()
run event_pipe = do
    let w = empty_world
    screen <- initialise_screen
    sprites <- initialise_sprites
    let cursor = initialise_cursor
    handle_events event_pipe w screen sprites cursor

initialise_cursor :: Cursor
initialise_cursor = (0, 0)

initialise_screen :: IO Surface
initialise_screen = do
    let w = fromIntegral screen_width
    let h = fromIntegral screen_height
    let d = fromIntegral screen_depth
    screen <- setVideoMode w h d screen_flags
    showCursor False
    return screen

handle_events :: (Chan ViewEvent.ViewEvent) -> World -> Surface -> Sprites -> Cursor -> IO ()
handle_events c w screen sprites cursor = do
    e <- readChan c
    case e of
        ViewEvent.UpdateMove m -> do
            let w' = apply_move m w
            handle_events c w' screen sprites cursor
        ViewEvent.PositionCursor x y -> do
            let cursor' = (x, y)
            handle_events c w screen sprites cursor'
        ViewEvent.Redraw -> do
            redraw_world w screen sprites cursor
            handle_events c w screen sprites cursor
        ViewEvent.End -> return ()

apply_move :: Game.Move -> World -> World
apply_move m w =
    case m `List.elem` w of
        True -> w -- ignore duplicate moves
        False -> (m:w)

redraw_world :: World -> Surface -> Sprites -> Cursor -> IO ()
redraw_world w screen sprites cursor = do
    draw_background screen
    draw_grid screen sprites w
    draw_cursor screen cursor
    SDL.flip screen

draw_background :: Surface -> IO ()
draw_background screen = do
    fillRect screen Nothing (Pixel screen_bg_colour_rgb)
    return ()

draw_cursor :: Surface -> Cursor -> IO ()
draw_cursor screen (x, y) = do
    let w = screen_cursor_size
    let h = w
    fillRect screen (Just (Rect x y w h)) (Pixel screen_cursor_colour_rgb)
    return ()

-- sprite drawing

draw_grid :: Surface -> Sprites -> [Game.Move] -> IO ()
draw_grid screen sprites ((k, i, j):xs) = do
    let w = fromIntegral sprite_width
    let h = fromIntegral sprite_height
    let i' = i - 1
    let j' = j - 1
    blitSurface (sprites Map.! k) Nothing screen (Just (Rect (i' * w) (j' * h) w h))
    draw_grid screen sprites xs
draw_grid screen sprites _ = return ()

-- sprite loading

load_sprite :: String -> IO Surface
load_sprite bmp_file_name = do
    sprite <- loadBMP bmp_file_name
    sprite' <- displayFormat sprite
    setColorKey sprite' [SrcColorKey] (Pixel transparent_sprite_colour_rgb)
    return sprite'

initialise_sprites :: IO Sprites
initialise_sprites = do
    let keys = [Game.O, Game.X]
    values <- mapM load_sprite ["sprite/o.bmp", "sprite/x.bmp"]
    return $ Map.fromList $ zip keys values


