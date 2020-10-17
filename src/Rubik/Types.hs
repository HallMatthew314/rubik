module Rubik.Types
( Color(..)
, Square(..)
, Face
, Cube(..)
, MoveAmount(..)
, MoveFace(..)
, Move(..)
, Rotation
, Algorithm
, plainFace
, faceFromList
, getColor
, setColor
, faceColors
, baseCube
, turnTimes
, isNormal
, isRotation
, isCombined
, parseAlgorithm
) where

import qualified Data.Map.Strict as M
import Text.Read (readMaybe)
import Text.Printf ( PrintfArg
                   , formatArg
                   , fmtChar
                   , vFmt
                   , formatString
                   , fmtPrecision
                   , errorBadFormat
                   )

data Color = Wh | Gr | Re | Bl | Or | Ye deriving (Show, Eq)

-- REVIEW:
-- I don't actually understand what's happening here, but it works.
instance PrintfArg Color where
  formatArg c fmt | fmtChar (vFmt 's' fmt) == 's' =
    formatString (show c) (fmt { fmtChar = 's', fmtPrecision = Nothing})
  formatArg _ fmt = errorBadFormat $ fmtChar fmt 

-- Each symbol is a different sticker/square.
-- C = corner, E = edge
-- The symbols correspond to the following squares on a face:
-- CA EA CB
-- ED -- EB
-- CD EC CC
data Square = CA | EA | CB | EB | CC | EC | CD | ED
  deriving (Show, Eq, Ord, Enum)

-- A face is a map from a Square to a Color
type Face = M.Map Square Color

-- Face objects should be instantiated with this
-- to ensure all key slots are filled.
plainFace :: Color -> Face
plainFace = faceFromList . replicate 8

-- Testing function
faceFromList :: [Color] -> Face
faceFromList cs
  | length cs == 8 = M.fromList $ zip [CA .. ED] cs
  | otherwise      = error "Face objects require exactly 8 Colors."

getColor :: Square -> Face -> Maybe Color
getColor = M.lookup

setColor :: Square -> Color -> Face -> Face
setColor = M.insert

faceColors :: Face -> [Color]
faceColors = M.elems

data Cube = Cube { up    :: Face
                 , front :: Face
                 , right :: Face
                 , back  :: Face
                 , left  :: Face
                 , down  :: Face
                 } deriving (Show)

-- Generates a solved cube.
baseCube :: Cube
baseCube = Cube { up    = plainFace Wh
                , front = plainFace Gr
                , right = plainFace Re
                , back  = plainFace Bl
                , left  = plainFace Or
                , down  = plainFace Ye
                }

-- Normal turns, cube rotations
-- and slices, which are a combination of the previous two.
data MoveFace = R | L | U | D | F | B
              | X | Y | Z
              | M | E | S
              deriving (Show, Read, Eq, Ord)

-- Normal turn, double turn, inverse turn.
data MoveAmount = A1 | A2 | A3 deriving (Eq)

instance Show MoveAmount where
  show A1 = ""
  show A2 = "2"
  show A3 = "'" 

turnTimes :: MoveAmount -> Int
turnTimes A1 = 1
turnTimes A2 = 2
turnTimes A3 = 3

data Move = Move MoveFace MoveAmount

instance Show Move where
  show (Move f a) = show f ++ show a

-- REVIEW:
-- Probably a nicer way to do this but it should be okay for now.
instance Read Move where
  readsPrec _ s = p $ trim s
    where
      trim = dropWhile (==' ')
      p []          = []
      p (x:'2':xs)  = [(Move (read [x]) A2, xs)]
      p (x:'\'':xs) = [(Move (read [x]) A3, xs)]
      p (x:xs)      = [(Move (read [x]) A1, xs)]

type Rotation = Move

isNormal :: Move -> Bool
isNormal (Move U _) = True
isNormal (Move D _) = True
isNormal (Move R _) = True
isNormal (Move L _) = True
isNormal (Move F _) = True
isNormal (Move B _) = True
isNormal _          = False

isRotation :: Move -> Bool
isRotation (Move X _) = True
isRotation (Move Y _) = True
isRotation (Move Z _) = True
isRotation _          = False

isCombined :: Move -> Bool
isCombined (Move M _) = True
isCombined (Move E _) = True
isCombined (Move S _) = True
isCombined _          = False

-- An algorithm is just a sequence of moves.
type Algorithm = [Move]

parseAlgorithm :: String -> Algorithm
parseAlgorithm = map read . words

