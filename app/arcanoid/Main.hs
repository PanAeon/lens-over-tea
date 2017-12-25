{-# LANGUAGE TemplateHaskell, Rank2Types #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad.State (State, execState, get)
import Control.Monad (when)
import qualified Data.List as L
import Data.Set (Set, member, empty, insert, delete)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import System.Random (randomRs, newStdGen)
import Data.Ord(comparing)

gameSize        = 300
windowWidth     = 800
windowHeight    = 600
ballRadius      = 0.02
speedIncrease   = 1.2
initialSpeed    = 0.6
paddleWidth     = 0.56
paddleHeight    = 0.02
brickWidth      = 0.195
brickHeight     = 0.115
paddleSpeed     = 1.3
textSize        = 0.001

data Arcanoid = Arcanoid
  {
    _ballPos   :: Point
  , _ballSpeed :: Vector
  , _paddle    :: Float
  , _gameStarted :: Bool
  , _bricks      :: [Point]
  , _speedVectors     :: [Vector]

  , _keys        :: Set Key
  }

makeLenses ''Arcanoid

_x :: Field1 s t a b => Lens s t a b
_x = _1

_y :: Field2 s t a b => Lens s t a b
_y = _2

initial :: Arcanoid
initial = Arcanoid (0, 0) (0, 0) 0 False createBricks [] empty

createBricks :: [Point]
createBricks = [(x,y) | i <- [0..9],
                        j <- [1..7],
                        let x = -1.0 + 0.2 * i + 0.1,
                        let y = 1.0 - 0.12 * j - 0.06]

update :: Float -> Arcanoid -> Arcanoid
update time = execState $ do
  updatePuddle time
  updateBall time
  checkCollisions


checkCollisions :: State Arcanoid ()
checkCollisions = do
  p <- get
  let (x,y) = p^.ballPos

  when (abs x >= edge) $
    ballSpeed._x %= negate

  when (y >= edge) $
    ballSpeed._y %= negate

  when (y <= -edge) $
    reset

  when (x >= p^.paddle - paddleWidth/2 && x <= p^.paddle + paddleWidth / 2 && y < -0.95 + (paddleHeight / 2)) $ do
    ballSpeed._y %= negate
    ballSpeed._x += 3*(x - p^.paddle)

  checkCollisionsWithBricks

  where
    edge = 1 - ballRadius

increaseSpeed :: Float -> Float
increaseSpeed v = (signum v) * min 1.6 ( abs v * 1.2)


checkCollisionsWithBricks :: State Arcanoid ()
checkCollisionsWithBricks = do
  p <- get
  let (x,y) = p^.ballPos
      isColliding (bx,by) = x >= bx - ballRadius - brickWidth / 2 &&
                            x <= bx + ballRadius + brickWidth / 2 &&
                            y >= by - ballRadius - brickHeight / 2 &&
                            y <= by + ballRadius + brickHeight / 2
      mbb = L.find isColliding $ p^.bricks
  case mbb of
    (Just (bx, by)) -> do
      let xs = [ (abs (bx - ballRadius - brickWidth / 2 - x), (-1,1))
               , (abs (bx + ballRadius + brickWidth / 2 - x), (-1,1))
               , (abs (by - ballRadius - brickHeight / 2 - y), (1,-1))
               , (abs (by + ballRadius + brickHeight / 2 - y), (1,-1))
               ]
          (dx,dy) = snd $ L.minimumBy (comparing fst) xs
      ballSpeed._x *= dx
      ballSpeed._y *= dy
      ballSpeed.both %= increaseSpeed
      bricks %= (L.delete (bx, by))
    Nothing  -> return ()
-- Reset the game
reset :: State Arcanoid ()
reset = do
  p <- get
  ballPos .= (p ^. paddle, ballInitY)
  bricks  .= createBricks
  gameStarted .= False
  ballSpeed <~ nextSpeed
  where
    ballInitY = -0.95 + (paddleHeight / 2) + (ballRadius) -- FIXME: dups 3 times!

-- Retrieve a speed from the list, dropping it in the process
nextSpeed :: State Arcanoid Vector
nextSpeed = do
  v:vs <- use speedVectors
  speedVectors .= vs
  return v

updateBall :: Float -> State Arcanoid ()
updateBall time = do
  p <- get
  (u, v) <- use ballSpeed
  if p ^.gameStarted
  then do
    ballPos += (time * u, time * v)
    ballPos.both %= clamp ballRadius
  else do
    ballPos .= (p ^. paddle, ballInitY)
  where
    ballInitY = -0.95 + (paddleHeight / 2) + (ballRadius)

updatePuddle :: Float -> State Arcanoid ()
updatePuddle time = do
  p <- get

  let paddleMovement = time * paddleSpeed
      keyPressed key = p^.keys.contains (SpecialKey key)
      gameStopped = not (p ^.gameStarted)

  when (keyPressed KeyRight) $ paddle += paddleMovement
  when (keyPressed KeyLeft) $ paddle -= paddleMovement
  when (keyPressed KeySpace && gameStopped) $ gameStarted .= True

  paddle %= clamp (paddleWidth / 2)

clamp :: Float -> Float -> Float
clamp pad = max (pad - 1) . min (1 - pad)

draw :: Arcanoid -> Picture
draw a = scale gameSize gameSize $ Pictures (
   [ drawBall  `at` a^.ballPos
   , drawPaddle `at` ( a^.paddle, paddleY)
   , rectangleWire 2 2
   ] ++ (drawBricks (a^.bricks)) )
   where
     paddleY = -0.95
     p `at` (x,y) = translate x y p; infixr 1 `at`

drawPaddle :: Picture
drawPaddle = rectangleSolid paddleWidth paddleHeight

drawBall :: Picture
drawBall = circleSolid ballRadius

drawBrick :: Picture
drawBrick = rectangleSolid brickWidth brickHeight

drawBricks :: [Point] -> [Picture]
drawBricks bs = fmap (\b -> drawBrick `at` b) bs
  where
    p `at` (x,y) = translate x y p; infixr 1 `at`

main :: IO ()
main = do
  v:vs <- startingSpeeds
  let world = ballSpeed .~ v $ speedVectors .~ vs $ initial
  play display backColor fps world draw handle update

  where
    display    = InWindow "Arcanoid!" (windowWidth, windowHeight) (200, 200)
    backColor = white
    fps       = 120

handle :: Event -> Arcanoid -> Arcanoid
handle (EventKey k s _ _) = keys.contains k .~ (s == Down)
handle _ = id

startingSpeeds :: IO [Vector]
startingSpeeds = do
  rs <- randomRs (0, initialSpeed) <$> newStdGen
  return . interleave $ filter ((> 0.3) . abs) rs

  where
    interleave :: [a] -> [(a,a)]
    interleave (x:y:xs) = (x,y) : interleave xs
interleave _ = []
