{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Data.Array
import Data.List.Split
import Data.List
import Data.Time.Clock.POSIX
import Data.Text (append, unpack)

import Prelude hiding (init)
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import Foreign.C.Types
import SDL.Vect
import SDL.Event
import qualified SDL

import Paths_haskis (getDataFileName)
--
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Foldable
#endif

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data GamePlayAction = Quit | Accelerate | Hammer | RotateLeft | RotateRight | MoveLeft | MoveRight deriving Eq
data BlockType = None | L | InvertedL | S | InvertedS | T | Line | Square deriving Show
data BlockOrientation = North | East | West | South
data BlockPosition = BP Int Int
type HaskisBlockData = Array (Int, Int) BlockType

blockPositionX :: BlockPosition -> Int
blockPositionX (BP x _) = x
blockPositionY :: BlockPosition -> Int
blockPositionY (BP _ y) = y

data Block = Block {
  blockType :: BlockType,
  blockPosition :: BlockPosition,
  blockOrientation :: BlockOrientation
}

data GameState = GameState {
  boardWidth :: Int,
  boardHeight :: Int,
  boardContent :: HaskisBlockData,
  currentBlock :: Maybe Block,
  currentBlockTimer :: Int,
  randomSeed :: Integer
}

makeT         :: HaskisBlockData
makeT         =  array ((0,0),(2,1)) [((0,0),None), ((1,0),T), ((2,0),None), ((0,1),T), ((1,1),T), ((2,1),T)]
makeS         :: HaskisBlockData
makeS         =  array ((0,0),(2,1)) [((0,0),None), ((1,0),S), ((2,0),S), ((0,1),S), ((1,1),S), ((2,1),None)]
makeInvertedS :: HaskisBlockData
makeInvertedS =  array ((0,0),(2,1)) [((0,0),S), ((1,0),S), ((2,0),None), ((0,1),None), ((1,1),S), ((2,1),S)]
makeL         :: HaskisBlockData
makeL         =  array ((0,0),(2,1)) [((0,0),L), ((1,0),L), ((2,0),L), ((0,1),L), ((1,1),None), ((2,1),None)]
makeInvertedL :: HaskisBlockData
makeInvertedL =  array ((0,0),(2,1)) [((0,0),L), ((1,0),None), ((2,0),None), ((0,1),L), ((1,1),L), ((2,1),L)]
makeLine      :: HaskisBlockData
makeLine      =  array ((0,0),(3,0)) [((0,0),Line), ((1,0),Line), ((2,0),Line), ((3,0),Line)]
makeSquare    :: HaskisBlockData
makeSquare    =  array ((0,0),(1,1)) [((0,0),Square), ((1,0),Square), ((0,1),Square), ((1,1),Square)]

makeBlock           :: BlockType -> HaskisBlockData
makeBlock L         =  makeL
makeBlock InvertedL =  makeInvertedL
makeBlock S         =  makeS
makeBlock InvertedS =  makeInvertedS
makeBlock T         =  makeT
makeBlock Line      =  makeLine
makeBlock Square    =  makeSquare

moveBlock :: BlockPosition -> HaskisBlockData -> HaskisBlockData
moveBlock (BP x y) block = ixmap ((x0+x,x1+y),(y0+x,y1+y))
    (\(i,j) -> (i-x, j-y))
    block
    where ((x0,x1),(y0,y1)) = bounds block

makeBlockDataFromBlock :: Block -> HaskisBlockData
makeBlockDataFromBlock block = case bo of
  North -> moveBlock bp $ makeBlock bt
  East  -> moveBlock bp $ rotateArrayRight $ makeBlock bt
  South -> moveBlock bp $ rotateArrayRight $ rotateArrayRight $ makeBlock bt
  West  -> moveBlock bp $ rotateArrayLeft $ makeBlock bt
  where bo = (blockOrientation block)
        bt = (blockType block)
        bp = (blockPosition block)

blockTypeToChar :: BlockType -> Char
blockTypeToChar None      = '.'
blockTypeToChar L         = 'L'
blockTypeToChar InvertedL = 'J'
blockTypeToChar S         = 'S'
blockTypeToChar InvertedS = 'Z'
blockTypeToChar T         = 'T'
blockTypeToChar Line      = 'L'
blockTypeToChar Square    = 'Q'

rotateArrayLeft :: HaskisBlockData -> HaskisBlockData
rotateArrayLeft arr = array ((x1,x0),(y1,y0))
                            (zip invertedIndices arrElements)
                      where ((x0,x1),(y0,y1)) = bounds arr
                            invertedIndices = [(i,(y0-j)) | j <- [x0..y0], i <- [x1..y1]]
                            arrElements = elems arr

rotateArrayRight :: HaskisBlockData -> HaskisBlockData
rotateArrayRight arr = array ((x1,x0),(y1,y0))
                             (zip invertedIndices arrElements)
                       where ((x0,x1),(y0,y1)) = bounds arr
                             invertedIndices = [((y1-i),j) | j <- [x0..y0], i <- [x1..y1]]
                             arrElements = elems arr

--blocksIntersect :: HaskisBlockData -> HaskisBlockData -> Bool
--blocksIntersect b1 b2 =
--blockCollidesWithBoard :: HaskisBlockData -> Block -> Bool
--blockCollidesWithBoard board block = blocksIntersect (makeBlockDataFromBlock block) board
blockCollidesWithBoard _ _ = False

blockNextPosition :: BlockPosition -> Bool -> BlockPosition
blockNextPosition a False = a
blockNextPosition (BP x y) True = BP x (y+1)

blockToStringList :: HaskisBlockData -> [String]
blockToStringList arr = chunksOf (y0+1) $ map (blockTypeToChar.(arr!)) elemsInOrder
                    where ((x0,x1),(y0,y1)) = bounds arr
                          elemsInOrder = [(i,j) | j <- [x1..y1], i <- [x0..y0]]

gameStartingWidth = 10
gameStartingHeight = 20
gameStartingBlockTimer = 30 -- TODO: Make it dependant on level
gameStartingOrientation = North

startingState :: Integer -> GameState
startingState seed = GameState {
  boardWidth   = gameStartingWidth,
  boardHeight  = gameStartingHeight,
  boardContent = array ((0,0), (gameStartingWidth,gameStartingHeight))
                       [((i,j), None) | i <- [0..gameStartingWidth],
                                        j <- [0..gameStartingHeight]],
  currentBlock = Just Block {
    blockType        = L,
    blockPosition    = BP 0 0,
    blockOrientation = North
  },
  currentBlockTimer = gameStartingBlockTimer,
  randomSeed = seed
}

keyInputs :: [Event] -> [GamePlayAction]
keyInputs events = map (fromJust) $
  filter (isJust) $
  map (\case
    SDL.QuitEvent -> Just Quit
    SDL.KeyboardEvent e ->
      if | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
            case SDL.keysymScancode (SDL.keyboardEventKeysym e) of
              SDL.ScancodeQ     -> Just Quit
              --SDL.ScancodeP     -> Just Pause
              SDL.ScancodeA     -> Just RotateRight
              SDL.ScancodeZ     -> Just RotateLeft
              SDL.ScancodeDown  -> Just Accelerate
              SDL.ScancodeSpace -> Just Hammer
              SDL.ScancodeLeft  -> Just MoveLeft
              SDL.ScancodeRight -> Just MoveRight
              _ -> Nothing
         | otherwise -> Nothing
    _ -> Nothing
  ) $
  map SDL.eventPayload events




data RenderPos = Centered | At (Point V2 CInt)

getPOSIXTimeSecs :: IO Integer
getPOSIXTimeSecs = round `fmap` getPOSIXTime

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path = do
  bmp <- SDL.loadBMP path
  SDL.createTextureFromSurface renderer bmp <* SDL.freeSurface bmp

renderTexture :: SDL.Renderer -> SDL.Texture -> RenderPos -> IO ()
renderTexture renderer tex pos = do
  ti <- SDL.queryTexture tex
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      pos'   = case pos of
                At p     -> p
                Centered -> let cntr a b = (a - b) `div` 2
                            in P $ V2 (cntr screenWidth w) (cntr screenHeight h)
      extent = V2 w h
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle pos' extent)

renderGameStateOnScreen :: GameState -> SDL.Renderer -> SDL.Texture -> IO ()
renderGameStateOnScreen gameState renderer image = do
  let imgPos' = V2 10 20
  renderTexture renderer image $ At (P imgPos')

--getDataFileName :: FilePath -> IO FilePath
--getDataFileName filename = do
--  basePath <- SDL.getBasePath
--  return $ (unpack basePath) ++ filename

main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]

  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }

  window   <- SDL.createWindow "Haskis" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  image    <- getDataFileName "assets/element_red_square.bmp" >>= loadTexture renderer
  startingTime <- getPOSIXTimeSecs

  let initialGameState = startingState startingTime

  let loop inputGameState = do
        events <- SDL.pollEvents
        let events' = keyInputs events
            quit = elem Quit events'
            executeTick        = (currentBlockTimer inputGameState) == 0
            currentBlockTimer' = if executeTick then gameStartingBlockTimer else ((currentBlockTimer inputGameState) - 1)
            blockCollides      = if executeTick then (blockCollidesWithBoard (currentBlock inputGameState) (boardContent inputGameState)) else False
            currentBlock'      = currentBlock inputGameState
            currentBlock''     = if (isJust currentBlock')
              then
                Just Block {
                  blockType =  blockType $ fromJust currentBlock',
                  blockPosition = blockNextPosition (blockPosition $ fromJust currentBlock') executeTick,
                  blockOrientation = blockOrientation $ fromJust currentBlock'
                }
              else Nothing
            nextBoardContent   = if executeTick && blockCollides then
                (boardContent inputGameState)//[((i + (blockPositionX $ blockPosition $ fromJust currentBlock'), j + (blockPositionY $ blockPosition $ fromJust currentBlock')), k) | ((i,j), k) <- assocs $ makeBlockDataFromBlock $ fromJust currentBlock']
              else
                boardContent inputGameState
            nextGameState = GameState {
              boardWidth   = gameStartingWidth,
              boardHeight  = gameStartingHeight,
              boardContent = nextBoardContent,
              currentBlock = currentBlock'',
              currentBlockTimer = currentBlockTimer',
              randomSeed = randomSeed inputGameState
            }

        SDL.clear renderer
        renderGameStateOnScreen inputGameState renderer image
        SDL.present renderer
        unless quit $ loop nextGameState

  loop initialGameState

  SDL.destroyTexture image
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
