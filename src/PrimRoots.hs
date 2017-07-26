{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module PrimRoots where

import           Data.Colour.Palette.ColorSet
import           Data.List                           (intersperse)
import           Data.List.Split
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

r = 0.1

spoke t n sty k = (origin ~~ (1 ^& 0))
  # rotateBy (nlerp t theta (theta/4))
  # applyStyle sty
  # dashingL [r,r] 0 # lc gray # lw thin
  where
    theta = fromIntegral k / fromIntegral n

primroots n = filter ((==1) . gcd n) [1 .. n]

pt c nm r s = circle r # fc c # lw none # named nm # translateX s

primRoots :: Double -> Int -> Diagram B
primRoots t = primRoots' t (repeat mempty)

primdots :: Double -> Double -> Colour Double -> Int -> Double -> Diagram B
primdots t r c n s = foldMap
  (\d -> pt c (n,d) r s # rotateBy (nlerp t (theta d) (theta d / 4)))
  (primroots n)
  where
    theta d = fromIntegral d / fromIntegral n

allprimdots :: Double -> Double -> [Colour Double] -> [Int] -> Diagram B
allprimdots t r cs ns = mconcat $
  zipWith
    (\c n -> primdots t r c n (fromIntegral n)
             <> foldMap (scale (fromIntegral n) . spoke t n (mempty # dashingL [1,0] 0)) (primroots n))
    cs ns

primRoots' :: Double -> [Style V2 Double] -> Int -> Diagram B
primRoots' t = primRoots'' t blue

primRoots'' :: Double -> Colour Double -> [Style V2 Double] -> Int -> Diagram B
primRoots'' t dotColor styles n = mconcat
  [ primdots t r dotColor n 1
  , circle 1
  , mconcat $ zipWith (spoke t n) styles [0 .. n-1]
  ]

primRoots20 :: Diagram B
primRoots20 =
  map (primRoots 0) [1 .. 20]
  # chunksOf 4
  # map (hsep 1)
  # vsep 1


------------------------------------------------------------

colors = map d3Colors1 [0..9]

primRootsC c n = primRoots'' 0 c (repeat mempty) n

phiSum n k =
  vsep 0.5
  [ zipWith primRootsC colors divs
    # chunksOf k
    # map (intersperse plus)
    # map (hcat' (with & catMethod .~ Distrib & sep .~ 1.5))
    # map centerX
    # vcat' (with & catMethod .~ Distrib & sep .~ 2.5)
  , text "=" # fontSizeL 0.5
  , mconcat
    [ mconcat $ zipWith (\c n -> primdots 0 r c n 1) colors divs
    , circle 1
    , mconcat $ zipWith (spoke 0 n) (repeat mempty) [0 .. n-1]
    ]
  ]
  where
    divs = filter ((==0) . (n `mod`)) [1..n]
    plus = text "+" # fontSizeL 0.5

------------------------------------------------------------

allRoots :: Int -> Double -> Diagram B
allRoots n t = mconcat
  [ alldots r blue n 1
  , circle 1
  , mconcat $ zipWith (spoke t n) (repeat mempty) [0 .. n-1]
  ]
  where
    theta d = fromIntegral d / fromIntegral n
    alldots r c n s = foldMap
      (\d -> pt c (n,d) r s # rotateBy (nlerp t (theta d) (theta d / 4)))
      [0 .. n-1]

nlerp :: Num n => n -> n -> n -> n
nlerp t a b = (1-t)*a + t*b
