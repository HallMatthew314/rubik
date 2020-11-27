module Rubik.Types
( Color(..)
, Square(..)
, Face
, Cube(..)
, MoveAmount(..)
, MoveFace(..)
, Move(..)
, Normal
, Rotation
, Combined
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
, parseAlgorithmMaybe
, parseAlgorithmEither
, invertMove
, invertAlgorithm
, decomposeCombined
) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust, isNothing)
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
data MoveAmount = A1 | A2 | A3 deriving (Eq, Ord)

instance Show MoveAmount where
  show A1 = ""
  show A2 = "2"
  show A3 = "'" 

turnTimes :: MoveAmount -> Int
turnTimes A1 = 1
turnTimes A2 = 2
turnTimes A3 = 3

data Move = Move MoveFace MoveAmount deriving (Ord, Eq)

instance Show Move where
  show (Move f a) = show f ++ show a

-- REVIEW:
-- Probably a nicer way to do this but it should be okay for now.
instance Read Move where
  readsPrec _ s = p $ dropWhile (==' ') s
    where
      p (x:xs) | isJust m =
        case xs of
          ('2':rest)  -> [(Move m' A2, rest)]
          ('\'':rest) -> [(Move m' A3, rest)]
          rest        -> [(Move m' A1, rest)]
        where
          m  = readMaybe [x] :: Maybe MoveFace
          m' = fromJust m
      p _ = []

type Normal   = Move
type Rotation = Move
type Combined = Move

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

-- Returns Nothing if there are any invalid moves in the string.
-- Otherwise returns Just the list of moves.
parseAlgorithmMaybe :: String -> Maybe Algorithm
parseAlgorithmMaybe = sequence . map readMaybe . words

-- Returns Left error message if there are any invalid moves in the string
-- Otherwise returns Right the list of moves
parseAlgorithmEither :: String -> Either String Algorithm
parseAlgorithmEither s = maybe err Right may
  where
    may = parseAlgorithmMaybe s
    err = Left $ "Unable to parse: " ++ s

-- Unsafe parse function, should be used when parsing pre-defined
-- strings, not for live input.
parseAlgorithm :: String -> Algorithm
parseAlgorithm = either error id . parseAlgorithmEither 

invertMove :: Move -> Move
invertMove (Move f A1) = Move f A3
invertMove (Move f A3) = Move f A1
invertMove m           = m 

invertAlgorithm :: Algorithm -> Algorithm
invertAlgorithm = foldl (\acc m -> invertMove m:acc) []

-- Accepts a variant of M, E or S and returns its OBTM equivalent.
decomposeCombined :: Combined -> Algorithm
decomposeCombined m@(Move f d)
  | isCombined m = [invertMove r, a, invertMove b]
  | otherwise    = error "Provided move was not a variant of M, E or S"
  where
    (r,a,b) = (Move r' d, Move a' d, Move b' d)
    (r',a',b') = case f of
      M -> (X,R,L)
      E -> (Y,U,D)
      S -> (Z,F,B)

