module Rubik.Utils where

import Rubik.Types
import Text.Printf (PrintfType, printf)

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
    [u1,u2,u3,u4,u5,u6,u7,u8] = faceColors u
    [f1,f2,f3,f4,f5,f6,f7,f8] = faceColors f
    [r1,r2,r3,r4,r5,r6,r7,r8] = faceColors r
    [b1,b2,b3,b4,b5,b6,b7,b8] = faceColors b
    [l1,l2,l3,l4,l5,l6,l7,l8] = faceColors l
    [d1,d2,d3,d4,d5,d6,d7,d8] = faceColors d

