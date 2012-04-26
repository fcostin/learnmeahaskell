module Game.WorldView (run) where

import Control.Concurrent.Chan
import qualified Data.Map as Map
import Data.Word (Word32)
import Graphics.UI.SDL as SDL

import qualified Game.ViewEvent as ViewEvent
import qualified Game.Board as Game
import qualified Game.World as World

screen_width :: Integer
screen_width = 150

screen_height :: Integer
screen_height = 150

screen_depth :: Integer
screen_depth = 32

screen_flags :: [SurfaceFlag]
screen_flags = [HWSurface, DoubleBuf]

screen_bg_colour_rgb :: Word32
screen_bg_colour_rgb = 0x000000

screen_cursor_colour_rgb :: Word32
screen_cursor_colour_rgb = 0xFFFFFF

screen_cursor_size :: Int
screen_cursor_size = 10

sprite_width :: Int
sprite_width = 50

sprite_height :: Int
sprite_height = 50


transparent_sprite_colour_rgb :: Word32
transparent_sprite_colour_rgb = 0xFF00FF

type Sprites = Map.Map Game.Piece Surface

type Cursor = (Int, Int)

run :: (Chan ViewEvent.ViewEvent) -> IO ()
run event_pipe = do
    screen <- initialise_screen
    sprites <- initialise_sprites
    let cursor = initialise_cursor
    handle_events event_pipe screen sprites cursor

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

handle_events :: (Chan ViewEvent.ViewEvent) -> Surface -> Sprites -> Cursor -> IO ()
handle_events c screen sprites cursor = do
    e <- readChan c
    case e of
        ViewEvent.PositionCursor x y -> do
            let cursor' = (x, y)
            handle_events c screen sprites cursor'
        ViewEvent.Redraw w -> do
            redraw_world w screen sprites cursor
            handle_events c screen sprites cursor
        ViewEvent.End -> return ()

redraw_world :: World.World -> Surface -> Sprites -> Cursor -> IO ()
redraw_world w screen sprites cursor = do
    draw_background screen
    draw_grid screen sprites w
    draw_cursor screen cursor
    SDL.flip screen

draw_background :: Surface -> IO ()
draw_background screen = do
    _ <- fillRect screen Nothing (Pixel screen_bg_colour_rgb)
    return ()

draw_cursor :: Surface -> Cursor -> IO ()
draw_cursor screen (x, y) = do
    let w = screen_cursor_size
    let h = w
    _ <- fillRect screen (Just (Rect x y w h)) (Pixel screen_cursor_colour_rgb)
    return ()

-- sprite drawing

draw_grid :: Surface -> Sprites -> [Game.Move] -> IO ()
draw_grid screen sprites ((k, i, j):xs) = do
    let w = fromIntegral sprite_width
    let h = fromIntegral sprite_height
    let i' = i - 1
    let j' = j - 1
    _ <- blitSurface (sprites Map.! k) Nothing screen (Just (Rect (i' * w) (j' * h) w h))
    draw_grid screen sprites xs
draw_grid _ _ _ = return ()

-- sprite loading

load_sprite :: String -> IO Surface
load_sprite bmp_file_name = do
    sprite <- loadBMP bmp_file_name
    sprite' <- displayFormat sprite
    _ <- setColorKey sprite' [SrcColorKey] (Pixel transparent_sprite_colour_rgb)
    return sprite'

initialise_sprites :: IO Sprites
initialise_sprites = do
    let keys = [Game.O, Game.X]
    values <- mapM load_sprite ["sprite/o.bmp", "sprite/x.bmp"]
    return $ Map.fromList $ zip keys values


