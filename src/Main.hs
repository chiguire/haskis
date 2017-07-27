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
(screenWidth, screenHeight) = (800, 600)

data GamePlayAction = Quit | Accelerate | Hammer | RotateLeft | RotateRight | MoveLeft | MoveRight deriving Eq
data BlockType = None | L | InvertedL | S | InvertedS | T | Line | Square deriving (Show, Eq)
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

data BlockImage = BlockImage {
  getLImage         :: SDL.Texture,
  getInvertedLImage :: SDL.Texture,
  getSImage         :: SDL.Texture,
  getInvertedSImage :: SDL.Texture,
  getTImage         :: SDL.Texture,
  getLineImage      :: SDL.Texture,
  getSquareImage    :: SDL.Texture,
  getNoneImage      :: SDL.Texture,
  getStarImage      :: SDL.Texture,
  getHeartImage     :: SDL.Texture
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

getBlockImage :: BlockImage -> BlockType -> SDL.Texture
getBlockImage bi L         =  getLImage         bi
getBlockImage bi InvertedL =  getInvertedLImage bi
getBlockImage bi S         =  getSImage         bi
getBlockImage bi InvertedS =  getInvertedSImage bi
getBlockImage bi T         =  getTImage         bi
getBlockImage bi Line      =  getLineImage      bi
getBlockImage bi Square    =  getSquareImage    bi
getBlockImage bi None      =  getNoneImage      bi

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

blocksIntersect :: HaskisBlockData -> HaskisBlockData -> Bool
blocksIntersect b1 b2 = any (intersect) $ zip b1values b2values
                        where intersect (x, y) = (x /= None) && (y /= None)
                              b1ix = indices b1
                              b1values = map ((!) b1) $ b1ix
                              b2values = map ((!) b2) $ b1ix

blockCollidesWithBoard :: Block -> HaskisBlockData -> Bool
blockCollidesWithBoard block board =
  (((blockPositionY $ blockPosition $ block) + 1) >= gameStartingHeight) ||
  (blocksIntersect (makeBlockDataFromBlock block) board)

moveDownIf :: Bool -> Block -> Block
moveDownIf condition block = Block {
  blockType =  blockType block,
  blockPosition = blockNextPosition condition $ blockPosition $ block,
  blockOrientation = blockOrientation block
} where blockNextPosition :: Bool -> BlockPosition -> BlockPosition
        blockNextPosition False a        = a
        blockNextPosition True  (BP x y) = BP x (y+1)

blockToStringList :: HaskisBlockData -> [String]
blockToStringList arr = chunksOf (y0+1) $ map (blockTypeToChar.(arr!)) elemsInOrder
                    where ((x0,x1),(y0,y1)) = bounds arr
                          elemsInOrder = [(i,j) | j <- [x1..y1], i <- [x0..y0]]

gameStartingWidth = 10
gameStartingHeight = 16
gameStartingBlockTimer = 300 -- TODO: Make it dependant on level
gameStartingOrientation = North

startingState :: Integer -> GameState
startingState seed = GameState {
  boardWidth   = gameStartingWidth,
  boardHeight  = gameStartingHeight,
  boardContent = array ((0,0), ((gameStartingWidth-1),(gameStartingHeight-1)))
                       [((i,j), None) | i <- [0..(gameStartingWidth-1)],
                                        j <- [0..(gameStartingHeight-1)]],
  currentBlock = Just Block {
    blockType        = Square,
    blockPosition    = BP 2 0,
    blockOrientation = gameStartingOrientation
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

createTexture :: SDL.Renderer -> Int -> Int -> IO SDL.Texture
createTexture renderer width height = do
  SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStatic (V2 1 1)

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

drawBlock :: SDL.Renderer -> BlockImage -> ((Int, Int), BlockType) -> IO ()
drawBlock renderer blockImage ((x,y),l) = do
  renderTexture renderer (getBlockImage blockImage l) $ At (P (V2 ((fromIntegral x)*32) ((fromIntegral y)*32)))

drawBlockSansNone :: SDL.Renderer -> BlockImage -> ((Int, Int), BlockType) -> IO ()
drawBlockSansNone renderer blockImage (pos, l) = do
  when (l /= None) $ drawBlock renderer blockImage (pos, l)

renderGameStateOnScreen :: GameState -> SDL.Renderer -> BlockImage -> Bool -> Bool -> IO ()
renderGameStateOnScreen gameState renderer blockImage executeTick blockCollides = do
  mapM_ (drawBlock renderer blockImage) $ assocs $ boardContent $ gameState
  when (isJust $ currentBlock gameState) $ mapM_ (drawBlockSansNone renderer blockImage) $ assocs $ makeBlockDataFromBlock $ fromJust $ currentBlock gameState
  when executeTick $ renderTexture renderer (getHeartImage blockImage) (At (P (V2 500 200)))
  when blockCollides $ renderTexture renderer (getStarImage blockImage) (At (P (V2 532 200)))
  --let imgPos' = V2 10 20
  --renderTexture renderer image $ At (P imgPos')

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

  lImage         <- getDataFileName "assets/element_blue_square.bmp"   >>= loadTexture renderer
  invertedLImage <- getDataFileName "assets/element_green_square.bmp"  >>= loadTexture renderer
  sImage         <- getDataFileName "assets/element_grey_square.bmp"   >>= loadTexture renderer
  invertedSImage <- getDataFileName "assets/element_orange_square.bmp" >>= loadTexture renderer
  tImage         <- getDataFileName "assets/element_purple_square.bmp" >>= loadTexture renderer
  lineImage      <- getDataFileName "assets/element_red_square.bmp"    >>= loadTexture renderer
  squareImage    <- getDataFileName "assets/element_yellow_square.bmp" >>= loadTexture renderer
  noneImage      <- getDataFileName "assets/element_none_square.bmp" >>= loadTexture renderer
  starImage      <- getDataFileName "assets/star.bmp" >>= loadTexture renderer
  heartImage     <- getDataFileName "assets/heart.bmp" >>= loadTexture renderer

  let blockImage = BlockImage {
    getLImage         = lImage,
    getInvertedLImage = invertedLImage,
    getSImage         = sImage,
    getInvertedSImage = invertedSImage,
    getTImage         = tImage,
    getLineImage      = lineImage,
    getSquareImage    = squareImage,
    getNoneImage      = noneImage,
    getStarImage      = starImage,
    getHeartImage     = heartImage
  }

  startingTime <- getPOSIXTimeSecs

  let initialGameState = startingState startingTime

  let loop inputGameState = do
        events <- SDL.pollEvents
        let events' = keyInputs events
            quit = elem Quit events'
            executeTick        = (currentBlockTimer inputGameState) == 0
            currentBlockTimer' = if executeTick then gameStartingBlockTimer else ((currentBlockTimer inputGameState) - 1)
            currentBlock'      = currentBlock inputGameState
            currentBlock''     = fmap (moveDownIf executeTick) currentBlock'
            blockCollides      = executeTick && (maybe False (\b -> blockCollidesWithBoard b $ boardContent $ inputGameState) currentBlock'')
            nextBoardContent   = boardContent inputGameState // (maybe [] (\b -> if executeTick && blockCollides then (assocs $ makeBlockDataFromBlock $ b) else []) currentBlock')
            nextGameState = GameState {
              boardWidth   = gameStartingWidth,
              boardHeight  = gameStartingHeight,
              boardContent = nextBoardContent,
              currentBlock = currentBlock'',
              currentBlockTimer = currentBlockTimer',
              randomSeed = randomSeed inputGameState
            }

        SDL.clear renderer
        renderGameStateOnScreen inputGameState renderer blockImage executeTick blockCollides
        SDL.present renderer
        unless quit $ loop nextGameState

  loop initialGameState

  SDL.destroyTexture lImage
  SDL.destroyTexture invertedLImage
  SDL.destroyTexture sImage
  SDL.destroyTexture invertedSImage
  SDL.destroyTexture tImage
  SDL.destroyTexture lineImage
  SDL.destroyTexture squareImage
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  SDL.quit
