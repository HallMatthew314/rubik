module Rubik where

import Text.Printf

data Color = Wh | Gr | Re | Bl | Or | Ye deriving (Show, Eq)

-- REVIEW:
-- I don't actually understand what's happening here, but it works.
instance PrintfArg Color where
  formatArg c fmt | fmtChar (vFmt 's' fmt) == 's' =
    formatString (show c) (fmt { fmtChar = 's', fmtPrecision = Nothing})
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

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

-- TODO:
-- There must be a better way to do this.
prettyPrintCube :: PrintfType r => Cube -> r
prettyPrintCube Cube {up=u, front=f, right=r, back=b, left=l, down=d} =
  printf template
    -- Up face
    (s1 u)
    (s2 u)
    (s3 u)
    (s8 u)
    (s4 u)
    (s7 u)
    (s6 u)
    (s5 u)
    -- First layer of middle faces (Left, Front, Right, Back)
    (s1 l)
    (s2 l)
    (s3 l)
    (s1 f)
    (s2 f)
    (s3 f)
    (s1 r)
    (s2 r)
    (s3 r)
    (s1 b)
    (s2 b)
    (s3 b)
    -- Second layer of middle faces
    (s8 l)
    (s4 l)
    (s8 f)
    (s4 f)
    (s8 r)
    (s4 r)
    (s8 b)
    (s4 b)
    -- Third Layer of middle faces
    (s7 l)
    (s6 l)
    (s5 l)
    (s7 f)
    (s6 f)
    (s5 f)
    (s7 r)
    (s6 r)
    (s5 r)
    (s7 b)
    (s6 b)
    (s5 b)
    -- Down face
    (s1 d)
    (s2 d)
    (s3 d)
    (s8 d)
    (s4 d)
    (s7 d)
    (s6 d)
    (s5 d)
  where
    template = unlines
            [ "       %s%s%s"
            , "       %s--%s"
            , "       %s%s%s\n"
            , "%s%s%s %s%s%s %s%s%s %s%s%s"
            , "%s--%s %s--%s %s--%s %s--%s"
            , "%s%s%s %s%s%s %s%s%s %s%s%s\n"
            , "       %s%s%s"
            , "       %s--%s"
            , "       %s%s%s"
            ]

