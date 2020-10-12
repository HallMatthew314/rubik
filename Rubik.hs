module Rubik where

data Color = Wh | Gr | Re | Bl | Or | Ye deriving (Show)

-- Each field is a different sticker/square.
-- The numbers correspond to the following squares on a face:
-- 1 2 3
-- 8 x 4
-- 7 6 5
data Face = Face { s1 :: Color
                 , s2 :: Color
                 , s3 :: Color
                 , s4 :: Color
                 , s5 :: Color
                 , s6 :: Color
                 , s7 :: Color
                 , s8 :: Color
                 } deriving (Show)

-- Squares can also be accessed through these functions:
-- C = corner, E = edge
faceCA = s1
faceEA = s2
faceCB = s3
faceEB = s4
faceCC = s5
faceEC = s6
faceCD = s7
faceED = s8

plainFace :: Color -> Face
plainFace c = Face c c c c c c c c

data Cube = Cube { up    :: Face
                 , front :: Face
                 , right :: Face
                 , back  :: Face
                 , left  :: Face
                 , down  :: Face
                 } deriving (Show)

baseCube :: Cube
baseCube = Cube { up    = plainFace Wh
                , front = plainFace Gr
                , right = plainFace Re
                , back  = plainFace Bl
                , left  = plainFace Or
                , down  = plainFace Ye
                }

