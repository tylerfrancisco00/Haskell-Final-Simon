module Main (main) where

import Graphics.Gloss

--
--      VISUALS
--
-- For building the GUI
-- each arc comes with 2 sets of degrees, a radius, and a thickness
-- each color comes with an RGBA value

window :: Display
window = InWindow "Simon!" (800, 800) (10, 10)

background = makeColorI 170 170 170 255

activeYellow = color $ makeColorI 255 234 55 125
passiveYellow = color $ makeColorI 255 234 55 255
arcYellow = thickArc 180 270 200 75

activeGreen = color $ makeColorI 62 221 75 125
passiveGreen = color $ makeColorI 62 221 75 255
arcGreen = thickArc 270 360 200 75

activeBlue = color $ makeColorI 75 62 221 125
passiveBlue = color $ makeColorI 75 62 221 255
arcBlue = thickArc 90 180 200 75

activeRed = color $ makeColorI 221 75 62 125
passiveRed = color $ makeColorI 221 75 62 255
arcRed = thickArc 0 90 200 75

-- Define a buttons data picture
data Buttons = Buttons
  { buttonYellow :: Picture,
    buttonGreen :: Picture,
    buttonBlue :: Picture,
    buttonRed :: Picture
  }

passiveButtons =
  Buttons
    { buttonYellow = passiveYellow $ arcYellow,
      buttonGreen = passiveGreen $ arcGreen,
      buttonBlue = passiveBlue $ arcBlue,
      buttonRed = passiveRed $ arcRed
    }


-- Displays all of the buttons based on the current game state
-- if a certain collor is "active" it is being displayed to the user
showStates :: State -> Picture
showStates gs = case colorOn gs of
        Just yellow -> (buttonsToPicture passiveButtons {buttonYellow = activeYellow $ arcYellow})
        Just green -> (buttonsToPicture passiveButtons {buttonGreen = activeGreen $ arcGreen})
        Just blue -> (buttonsToPicture passiveButtons {buttonBlue = activeBlue $ arcBlue})
        Just red -> (buttonsToPicture passiveButtons {buttonRed = activeRed $ arcRed})
        Nothing -> (buttonsToPicture passiveButtons)

buttonsToPicture :: Buttons -> Picture
buttonsToPicture bp = pictures [ 
                                buttonYellow bp,
                                buttonGreen bp,
                                buttonBlue bp,
                                buttonRed bp
                                ]

-- A funciton that returns a collection of pictures making up the buttons 
build :: Buttons -> Picture
build bp =
    pictures [  buttonYellow bp,
                buttonGreen bp,
                buttonBlue bp,
                buttonRed bp
            ]

-- turns a state into an image
renderState :: State -> Picture
renderState gs = pictures [ showStates gs
                           ]

--
-- Game
--
-- Everything needed to process the game and load states

data State = State
  { colorOn :: Maybe ButtonColor,
    timer :: Int,
    status :: Status,
    colors :: [ButtonColor],
    sequece :: Int,
    mousePosition   :: Int } deriving Show

data ButtonColor = Yellow
                 | Green
                 | Blue
                 | Red
  deriving (Show, Eq)

data Status = InProgress
                | TakingInput
                | Finished
                | Loss
  deriving (Show, Eq)

initialState :: State
initialState = State
  { colorOn = Nothing,
    timer = 100,
    status = InProgress,
    colors = [],
    sequece = 1,
    mousePosition = 0
  }



drawing :: Picture
drawing = pictures
    [ color red $ thickArc 0 90 200 75,
      color blue $ thickArc 90 180 200 75,
      color yellow $ thickArc 180 270 200 75,
      color green $ thickArc 270 360 200 75,
      translate (-75) (-45) $ text "12" -- for the score
    ]

main :: IO ()
main = display window background drawing