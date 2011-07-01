module Main where

import Control.Applicative
import Control.Monad

import Graphics.UI.SDL

import Life
import qualified Data.Set as Set

start :: CoordSet
start = Set.fromList [
    (25,0),
    (22,1),(23,1),(24,1),(25,1),(30,1),
    (13,2),(21,2),(22,2),(23,2),(24,2),(30,2),
    (12,3),(14,3),(21,3),(24,3),(34,3),(35,3),
    (11,4),(15,4),(16,4),(21,4),(22,4),(23,4),(24,4),(34,4),(35,4),
    (0,5),(1,5),(11,5),(15,5),(16,5),(22,5),(23,5),(24,5),(25,5),
    (0,6),(1,6),(11,6),(15,6),(16,6),(25,6),
    (12,7),(14,7),
    (13,8)
  ]

main = withInit [InitEverything] $ do -- withInit calls quit for us.

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    setCaption "Life initializing" []

    bg_color  <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00
    fg_color  <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
    clip_rect <- Just <$> (getClipRect screen)

    board_loop start screen clip_rect bg_color fg_color

 where
    screenWidth  = 1280
    screenHeight = 960
    screenBpp    = 32


board_loop board screen clip_rect bg_color fg_color = do
    fillRect screen clip_rect bg_color
    mapM draw_cell (Set.toList board)

    Graphics.UI.SDL.flip screen

    quit <- while_events
    unless quit (board_loop (next_board board) screen clip_rect bg_color fg_color)
  where
    draw_cell coord = fillRect screen (cell_rect coord) fg_color

    cell_rect (x,y) = Just $ Rect { rectX=x*size, rectY=y*size, rectW=size, rectH=size }
      where size = 5

    while_events = do
	event <- pollEvent
	case event of
	    Quit    -> return True
	    NoEvent -> return False
	    _       -> while_events
