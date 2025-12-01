module Renderer where

import LSystems
import IC.Colour
import IC.Graphics
import Examples -- to draw it in for REPL auto-import

drawLSystem1 :: Bool -> LSystem -> Int -> Colour -> IO ()
drawLSystem1 liveAction system n colour =
  drawLines liveAction (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: Bool -> LSystem -> Int -> Colour -> IO ()
drawLSystem2 liveAction system n colour =
  drawLines liveAction (trace2 (expandLSystem system n) (angle system) colour)
