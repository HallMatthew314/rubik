module Rubik.Rotations
( turn
, applyAlgorithm
) where

import Rubik.Types

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
    cs  = faceColors f
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
    cFromS s m = case getColor s m of
      (Just c) -> c
      Nothing  -> error ("Missing square" ++ show s)
    a' = setColor as (cFromS ds d) a
    b' = setColor bs (cFromS as a) b
    c' = setColor cs (cFromS bs b) c
    d' = setColor ds (cFromS cs c) d

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

