module Rubik where

import qualified Data.Map.Strict as M
import Text.Printf
import Text.Read (readMaybe)

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

-- Variants will be added after basic turn function's implementation.
-- Rotations/slices will be added at a later date.
data Move = U | U2 | U'
          | F | F2 | F'
          | R | R2 | R'
          | B | B2 | B'
          | L | L2 | L'
          | D | D2 | D'
          deriving (Show, Read)

type Algorithm = [Move]

parseAlgorithm :: String -> Algorithm
parseAlgorithm = map read . words

-- Effects of one clockwise rotation:

-- Main face squares rotate 90 degrees:

-- This    becomes   this
-- CA EA CB          CD ED CA
-- ED    EB          EC    EA
-- CD EC CC          CC EB CB

-- Side squares rotate as follows:

-- U

-- F  -> L  -> B  -> R  -> F
-- CA    CA    CA    CA
-- EA    EA    EA    EA
-- CB    CB    CB    CB

-- D
-- F  -> R  -> B  -> L  -> F
-- CD    CD    CD    CD
-- EC    EC    EC    EC
-- CC    CC    CC    CC

-- F
-- U  -> R  -> D  -> L  -> U
-- CD    CA    CB    CC
-- EC    ED    EA    EB
-- CC    CD    CA    CB

-- B
-- U  -> L  -> D  -> R  -> U
-- CB    CA    CD    CC
-- EA    ED    EC    EB
-- CA    CD    CC    CB

-- R
-- U  -> B  -> D  -> F  -> U
-- CC    CA    CC    CC
-- EB    ED    EB    EB
-- CB    CD    CB    CB

-- L
-- U  -> F  -> D  -> B  -> U
-- CA    CA    CA    CC
-- ED    ED    ED    EB
-- CD    CD    CD    CB

-- More generally,

-- S
-- Na -> Nb -> Nc -> Nd -> Na
-- a1    b1    c1    d1
-- a2    b2    c2    d2
-- a3    b3    c3    d3

type SideSpecs = (RSpec,RSpec,RSpec,RSpec)
type RSpec = (Face,[Square])
type SCPair = (Square,Color)

baseRotateFace :: Face -> Face
baseRotateFace f = faceFromList cs'
  where
    cs  = M.elems f
    cs' = drop 6 cs ++ take 6 cs

baseRotateSides :: SideSpecs -> (Face,Face,Face,Face)
baseRotateSides ((a,[]),(b,[]),(c,[]),(d,[])) = (a,b,c,d)
baseRotateSides ( (a, (as:ar))
                , (b, (bs:br))
                , (c, (cs:cr))
                , (d, (ds:dr))
                ) = baseRotateSides ((a',ar),(b',br),(c',cr),(d',dr))
  where
    cFromS :: Square -> Face -> Color
    cFromS s m = case M.lookup s m of
      (Just c) -> c
      Nothing  -> error ("Missing square" ++ show s)
    a' = M.insert as (cFromS ds d) a
    b' = M.insert bs (cFromS as a) b
    c' = M.insert cs (cFromS bs b) c
    d' = M.insert ds (cFromS cs c) d

baseRotateRow :: (SCPair,SCPair,SCPair,SCPair) -> (SCPair,SCPair,SCPair,SCPair)
baseRotateRow ((sa,a),(sb,b),(sc,c),(sd,d)) = ((sa,d),(sb,a),(sc,b),(sd,c))

baseRotate :: Face -> SideSpecs -> (Face,Face,Face,Face,Face)
baseRotate f s = (f', a', b', c', d')
  where
    f' = baseRotateFace f
    (a',b',c',d') = baseRotateSides s

baseRotateU :: Cube -> Cube
baseRotateU c = c {up=u',front=f',left=l',back=b',right=r'}
  where
    (u',f',l',b',r') = baseRotate (up c) ( (front c,[CA,EA,CB])
                                         , (left  c,[CA,EA,CB])
                                         , (back  c,[CA,EA,CB])
                                         , (right c,[CA,EA,CB])
                                         )

baseRotateD :: Cube -> Cube
baseRotateD c = c {down=d',front=f',right=r',back=b',left=l'}
  where
    (d',f',r',b',l') = baseRotate (down c) ( (front c,[CD,EC,CC])
                                           , (right c,[CD,EC,CC])
                                           , (back  c,[CD,EC,CC])
                                           , (left  c,[CD,EC,CC])
                                           )

baseRotateF :: Cube -> Cube
baseRotateF c = c {front=f',up=u',right=r',down=d',left=l'}
  where
    (f',u',r',d',l') = baseRotate (front c) ( (up    c,[CD,EC,CC])
                                            , (right c,[CA,ED,CD])
                                            , (down  c,[CB,EA,CA])
                                            , (left  c,[CC,EB,CB])
                                            )

baseRotateB :: Cube -> Cube
baseRotateB c = c {back=b',up=u',left=l',down=d',right=r'}
  where
    (b',u',l',d',r') = baseRotate (back c) ( (up    c,[CB,EA,CA])
                                           , (left  c,[CA,ED,CD])
                                           , (down  c,[CD,EC,CC])
                                           , (right c,[CC,EB,CB])
                                           )

baseRotateR :: Cube -> Cube
baseRotateR c = c {right=r',up=u',back=b',down=d',front=f'}
  where
    (r',u',b',d',f') = baseRotate (right c) ( (up    c,[CC,EB,CB])
                                            , (back  c,[CA,ED,CD])
                                            , (down  c,[CC,EB,CB])
                                            , (front c,[CC,EB,CB])
                                            )

baseRotateL :: Cube -> Cube
baseRotateL c = c {left=l',up=u',front=f',down=d',back=b'}
  where
    (l',u',f',d',b') = baseRotate (left c) ( (up    c,[CA,ED,CD])
                                           , (front c,[CA,ED,CD])
                                           , (down  c,[CA,ED,CD])
                                           , (back  c,[CC,EB,CB])
                                           )

turn :: Move -> Cube -> Cube
turn U  = baseRotateU
turn U2 = baseRotateU . baseRotateU
turn U' = baseRotateU . baseRotateU . baseRotateU
turn F  = baseRotateF
turn F2 = baseRotateF . baseRotateF
turn F' = baseRotateF . baseRotateF . baseRotateF
turn R  = baseRotateR
turn R2 = baseRotateR . baseRotateR
turn R' = baseRotateR . baseRotateR . baseRotateR
turn B  = baseRotateB
turn B2 = baseRotateB . baseRotateB
turn B' = baseRotateB . baseRotateB . baseRotateB
turn L  = baseRotateL
turn L2 = baseRotateL . baseRotateL
turn L' = baseRotateL . baseRotateL . baseRotateL
turn D  = baseRotateD
turn D2 = baseRotateD . baseRotateD
turn D' = baseRotateD . baseRotateD . baseRotateD

applyAlgorithm :: Algorithm -> Cube -> Cube
applyAlgorithm a c = foldl (flip turn) c a

aperm = parseAlgorithm "F R U' R' U' R U R' F' R U R' U' R' F R F'"
testalg = [U2, F2, R2, B2, L2, D2]

-- TODO:
-- There must be a better way to do this.
prettyPrintCube :: PrintfType r => Cube -> r
prettyPrintCube Cube {up=u, front=f, right=r, back=b, left=l, down=d} =
  printf template
    -- Up face
    u1
    u2
    u3
    u8
    u4
    u7
    u6
    u5
    -- First layer of middle faces (Left, Front, Right, Back)
    l1
    l2
    l3
    f1
    f2
    f3
    r1
    r2
    r3
    b1
    b2
    b3
    -- Second layer of middle faces
    l8
    l4
    f8
    f4
    r8
    r4
    b8
    b4
    -- Third Layer of middle faces
    l7
    l6
    l5
    f7
    f6
    f5
    r7
    r6
    r5
    b7
    b6
    b5
    -- Down face
    d1
    d2
    d3
    d8
    d4
    d7
    d6
    d5
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
    [u1,u2,u3,u4,u5,u6,u7,u8] = M.elems u
    [f1,f2,f3,f4,f5,f6,f7,f8] = M.elems f
    [r1,r2,r3,r4,r5,r6,r7,r8] = M.elems r
    [b1,b2,b3,b4,b5,b6,b7,b8] = M.elems b
    [l1,l2,l3,l4,l5,l6,l7,l8] = M.elems l
    [d1,d2,d3,d4,d5,d6,d7,d8] = M.elems d
