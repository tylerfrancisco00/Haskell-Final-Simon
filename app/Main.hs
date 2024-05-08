module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random



window :: Display
window = InWindow "Simon!!" (800, 800) (100, 100)


-- Play is a function that takes a number of inputs
-- window -> the space the game is played in
-- Backgrond -> the backround color
-- fps -> The number of processes needed for a second to pass
-- initialState -> the initial state of the game world
-- renderState -> Contols how we see the world
-- handleInput -> Controls how we handle input
-- updateState -> Happens every program cycle
main :: IO ()
main = do
    gen <- newStdGen
    let rndColorSeq  = genColorSeq gen 5
    play window background 60 initialState {colors = rndColorSeq} renderState handleInput updateState

--
--      VISUALS
--

-- For building the GUI
-- each arc comes with 2 sets of degrees, a radius, and a thickness
-- each color comes with an RGBA value


background = makeColorI 170 170 170 255

activeYellow = color $ makeColorI 255 234 55 125
passiveYellow = color $ makeColorI 255 234 55 255
arcYellow = thickArc 180 270 200 75

activeGreen = color $ makeColorI 75 62 221 125
passiveGreen = color $ makeColorI 75 62 221 255
arcGreen = thickArc 270 360 200 75

activeBlue = color $ makeColorI 62 221 75 125
passiveBlue = color $ makeColorI 62 221 75 255
arcBlue = thickArc 90 180 200 75

activeRed = color $ makeColorI 221 75 62 125
passiveRed = color $ makeColorI 221 75 62 255
arcRed = thickArc 0 90 200 75

-- Define a buttons data with pictures
data Buttons = Buttons
  { buttonYellow :: Picture,
    buttonGreen :: Picture,
    buttonBlue :: Picture,
    buttonRed :: Picture
  }

-- The collection of buttons where there is no active button
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
showStates s = case active s of
        Just Yellow -> (buttonsToPicture passiveButtons {buttonYellow = activeYellow $ arcYellow})
        Just Green -> (buttonsToPicture passiveButtons {buttonGreen = activeGreen $ arcGreen})
        Just Blue -> (buttonsToPicture passiveButtons {buttonBlue = activeBlue $ arcBlue})
        Just Red -> (buttonsToPicture passiveButtons {buttonRed = activeRed $ arcRed})
        Nothing -> (buttonsToPicture passiveButtons)

-- 
buttonsToPicture :: Buttons -> Picture
buttonsToPicture bp = pictures [ 
                                buttonYellow bp,
                                buttonGreen bp,
                                buttonBlue bp,
                                buttonRed bp
                                ]

-- Changes the center display based on the current game state
displayScore :: State -> Picture
displayScore s = case status s of
                 Loss -> scale 0.2 0.2 $ translate (-310) (-35) $ color black $ text $ "Try Again"
                 Win -> scale 0.2 0.2 $ translate (-310) (-35) $ color black $ text $ "You Won!"
                 _        -> scale 0.3 0.3 $ translate (-35) (-45) $ color black $ text $ show (sequenceLength s - 1)


-- turns a state into an image
renderState :: State -> Picture
renderState s = pictures [ showStates s,
                            displayScore s
                           ]

--
--      GAME STATES
--
-- Everything needed to process the game and load states

-- All of the different colors
data ButtonColor = Yellow
                 | Green
                 | Blue
                 | Red
  deriving (Show, Eq)

-- All of the different game status 
data Status =   InProgress
                | TakingInput
                | Win
                | Loss
  deriving (Show, Eq)


-- Every game state needs all of these things
-- active           -> The current display state, if a button is active
-- A timer          -> The current time between color displays
-- A status         -> What is currently happening
-- colors           -> The current color sequence
-- sequence         -> The length of the current sequence
-- mousePosition    -> Where the mouse currently is
data State = State
  { active :: Maybe ButtonColor,
    time :: Int,
    status :: Status,
    colors :: [ButtonColor],
    sequenceLength :: Int,
    currentPos :: Int } deriving Show


-- The initial load state of a game, every game starts here
initialState :: State
initialState = State
  { active = Nothing,
    time = 100,
    status = InProgress,
    colors = [],
    sequenceLength = 1,
    currentPos = 0
  }

--
--      LOGIC
--
-- Changing game states, recording player input, and recording the games progress (adding to the list)

updateState :: Float -> State -> State
updateState _ s   | time s {time = (time s) - 1} <= 0   = nextState s
                  | otherwise                           = s {time = (time s) - 1}

nextState :: State -> State
nextState s   | status s == InProgress    = playColors s      -- If we need to give the user the color list
              | status s == TakingInput   = recieveColors s   -- if we need to recieve the color list
              | otherwise                 = s                 -- If we win or lose there is no state to advance

playColors :: State -> State
playColors s  | active s /= Nothing             = setActive Nothing 30 s                                        -- there is no color to display
              | currentPos s < sequenceLength s = currentIncrement $ setActive (Just $ getActive s) 30 s        -- display the next color in the sequence
              | otherwise                       = changeToTakingInput $ currentReset $ setActive Nothing 300 s  -- we have finished displaying the colors, change the state and begin to take input



recieveColors :: State -> State
recieveColors s | isValidColor s = if (currentPos s + 1 < sequenceLength s) -- check to make sure we have recieved a valid color
                                      then currentIncrement $ setActive Nothing 300 s -- is the currentPos is still less than the sequence length, increment and change the color
                                      else incrementSequencePosition $ changeToInProgress $ currentReset $ setActive Nothing 30 s -- if we have gone through all of the colors reset the state
                | otherwise      = changeToLoss s


-- reads the location of the mouse, translates it to a color and changes the display based on the input 
handleInput :: Event -> State -> State
handleInput (EventKey (MouseButton LeftButton) Up _ mousePos) s = case status s of
                                                                    TakingInput -> setActive (Just color) 30 s -- if we are taking input, change the color
                                                                    Loss  -> initialState {colors = genColorSeq (mkStdGen $ sequenceLength s) 5} -- if we are under a loss reset the game
                                                                    _         -> s -- if neither just return the state
  where
    color   = mouseToColor mousePos
handleInput _                                                 s = s

--
--      HELPER FUNCTIONS FOR LOGIC
--

-- A Converter from integers to button colors

-- changes the mouse position to a color    (DONE)
mouseToColor :: (Float, Float) -> ButtonColor
mouseToColor (x, y) | x < 0 && y < 0 = Yellow
                    | x > 0 && y < 0 = Green
                    | x > 0 && y > 0 = Red
                    | x < 0 && y > 0 = Blue

-- changes a integer (from the list) to a color
integerToColor :: Int -> ButtonColor
integerToColor n  | n == 1 = Yellow
                  | n == 2 = Green
                  | n == 3 = Red
                  | n == 4 = Blue

-- A function to generate a color sequence
genColorSeq :: StdGen -> Int -> [ButtonColor]
genColorSeq generator length = map integerToColor (take length $ randomRs (1,4) generator)

-- increments the length of sequence or ends the game if the sequence is greater than 10
incrementSequencePosition :: State -> State
incrementSequencePosition s | (sequenceLength s + 1) > 5   = changeToWin s
                            | otherwise             = s {sequenceLength = sequenceLength s + 1, status = InProgress}

-- increments the sequence position
currentIncrement :: State -> State
currentIncrement s = s {currentPos = currentPos s + 1}

-- A number of helper functions to change the gameState
changeToInProgress :: State -> State
changeToInProgress s = s {status = InProgress}

changeToTakingInput :: State -> State
changeToTakingInput s = s {status = TakingInput}

changeToWin :: State -> State
changeToWin s = s {status = Win, active = Nothing}

changeToLoss :: State -> State
changeToLoss s = s {status = Loss, active = Nothing}

-- setting the active color 
setActive :: Maybe ButtonColor -> Int -> State -> State
setActive color time s = s {active = color, time = time}

-- resets the mouse position
currentReset :: State -> State
currentReset s = s {currentPos = 0}

-- Checks if the color entered is valid 
isValidColor :: State -> Bool
isValidColor s = active s == (Just $ getActive s)
  
-- Retrieves the active color 
getActive :: State -> ButtonColor
getActive s = (colors s) !! (currentPos s)
