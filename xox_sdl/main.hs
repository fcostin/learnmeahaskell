import Prelude
import Graphics.UI.SDL as SDL
import Data.Word
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Xox as Xox

screen_width = 150
screen_height = screen_width
sprite_width = screen_width `quot` 3
sprite_height = screen_height `quot` 3

type SpriteMap = Map.Map Xox.Piece Surface

type SpriteCoord = (Xox.Piece, Int, Int)

clamp :: Ord a => a -> a -> a -> a
clamp x a b = max (min x b) a

make_coord :: Word16 -> Word16 -> SpriteCoord
make_coord mouse_x mouse_y = let
        i = mouse_x `quot` sprite_width
        j = mouse_y `quot` sprite_height
        i' = fromIntegral $ clamp i 0 2
        j' = fromIntegral $ clamp j 0 2
    in
        (Xox.X, i', j') -- player is always X

same_tile :: SpriteCoord -> SpriteCoord -> Bool
same_tile (_, i, j) (_, i', j') =
    (i == i') && (j == j')

process_ai_move :: [SpriteCoord] -> [SpriteCoord]
process_ai_move coords = let
        f (a, b, c) = ((b + 1, c + 1), a)
        f' = Map.fromList . map f
        board = f' coords
        (outcome, tile) = Xox.minimax Xox.O board
    in
        case tile of
            Just (i, j) -> (Xox.O, i - 1, j - 1):coords
            Nothing -> coords

process_player_move :: [SpriteCoord] -> SpriteCoord -> [SpriteCoord]
process_player_move coords c =
    case List.find (same_tile c) coords of
        Just _ -> coords
        Nothing -> let
                coords' = c:coords
                coords'' = process_ai_move coords'
            in
                coords''

load_sprite :: String -> IO Surface
load_sprite bmp_file_name = do
    sprite <- loadBMP bmp_file_name
    sprite' <- displayFormat sprite
    setColorKey sprite' [SrcColorKey] (Pixel 0xFF00FF)
    return sprite'

load_sprite_map :: IO SpriteMap
load_sprite_map = do
    let keys = [Xox.O, Xox.X]
    values <- mapM load_sprite ["sprite/o.bmp", "sprite/x.bmp"]
    return $ Map.fromList $ zip keys values

main = do
    SDL.init [InitEverything]
    screen <- setVideoMode (fromIntegral screen_width) (fromIntegral screen_height) 32 [HWSurface, DoubleBuf]
    showCursor False
    sprites <- load_sprite_map
    let mouse_x = screen_width `quot` 2
    let mouse_y = screen_height `quot` 2
    draw_and_update screen sprites [] (fromIntegral mouse_x) (fromIntegral mouse_y)

draw_background :: Surface -> IO ()
draw_background screen = do
    fillRect screen Nothing (Pixel 0x000000)
    return ()

draw_cursor :: Surface -> Int -> Int -> IO ()
draw_cursor screen x y = do
    fillRect screen (Just (Rect x y 10 10)) (Pixel 0xFFFFFF)
    return ()

draw_grid :: Surface -> SpriteMap -> [SpriteCoord] -> IO ()
draw_grid screen sprites ((k, i, j):xs) = do
    let w = fromIntegral sprite_width
    let h = fromIntegral sprite_height
    blitSurface (sprites Map.! k) Nothing screen (Just (Rect (i * w) (j * h) w h))
    let c = if (k == Xox.X) then "X" else "O"
    draw_grid screen sprites xs
draw_grid screen sprites _ = return ()

draw_and_update :: Surface -> SpriteMap -> [SpriteCoord] -> Word16 -> Word16 -> IO ()
draw_and_update screen sprites coords x y = do
    draw_background screen
    draw_grid screen sprites coords
    draw_cursor screen (fromIntegral x) (fromIntegral y)
    SDL.flip screen
    e <- waitEvent
    case e of
        MouseMotion x' y' _ _ -> draw_and_update screen sprites coords x' y'
        MouseButtonUp x' y' ButtonLeft -> do
            let coord = make_coord x' y'
            let coords' = process_player_move coords coord
            draw_and_update screen sprites coords' x' y'
        KeyDown (Keysym SDLK_ESCAPE _ _) -> return ()
        Quit -> return ()
        _ -> draw_and_update screen sprites coords x y
