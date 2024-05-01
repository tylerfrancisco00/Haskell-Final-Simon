module Main(main) where

import Graphics.Gloss

window :: Display
window = InWindow "Nice Window" (600, 600) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing