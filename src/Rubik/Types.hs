module Rubik.Types
( Color(..)
, Square(..)
, Face
, Cube(..)
, Move(..)
, Rotation
, Algorithm
, plainFace
, faceFromList
, getColor
, setColor
, faceColors
, baseCube
, isNormal
, isRotation
, parseAlgorithm
) where

import qualified Data.Map.Strict as M
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

          -- Normal moves
data Move = U | U2 | U'
          | F | F2 | F'
          | R | R2 | R'
          | B | B2 | B'
          | L | L2 | L'
          | D | D2 | D'
          -- Rotations (Translators of moves)
          | X | X2 | X'
          | Y | Y2 | Y'
          | Z | Z2 | Z'
          deriving (Eq, Ord, Show, Read)

type Rotation = Move

isNormal :: Move -> Bool
isNormal m = elem m [U,U2,U',F,F2,F',R,R2,R',B,B2,B',L,L2,L',D,D2,D']

isRotation :: Move -> Bool
isRotation m = elem m [X,X2,X',Y,Y2,Y',Z,Z2,Z']

-- An algorithm is just a sequence of moves.
type Algorithm = [Move]

parseAlgorithm :: String -> Algorithm
parseAlgorithm = map read . words

