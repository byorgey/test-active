{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Data.Coerce
import qualified Data.Map                            as M

import           Data.Colour.Palette.BrewerSet
import           Data.Colour.Palette.ColorSet
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

import           PrimRoots

primStars :: Rational -> Rational -> Diagram B
primStars t so =
  myallprimdots (fromRational t) (fromRational so)
    0.5 (map rybColor [0..23]) [1..24]

myallprimdots :: Double -> Double -> Double -> [Colour Double] -> [Int] -> Diagram B
myallprimdots t so r cs ns = mconcat $
  zipWith
    (\c n ->
       mconcat
       [ primdots t r c n (fromIntegral n)
       , foldMap
           ( scale (fromIntegral n)
           . spoke t n (mempty # dashingL [1,0] 0)
           )
           (primroots n)
         # opacity so
       ]
    )
    cs ns

allprimdots2 :: [Colour Double] -> [Int] -> Diagram B
allprimdots2 cs ns = mconcat $ zipWith primdots2 cs ns

primdots2 :: Colour Double -> Int -> Diagram B
primdots2 c n = foldMap
  (\d -> pt c (n,d) 0.5 (fromIntegral n) # translate (fromIntegral d *^ ((-1) ^& 1)))
  (primroots n)


cosRamp :: ActF Double
cosRamp = ui <#> \t -> (-cos (fromRational t * pi) + 1)/2

ramp :: ActF Rational
ramp = ui <#> \t -> (((-20 * t + 70) * t - 84) * t + 35) * t^4

anim :: ActF (Diagram B)
anim =
  movie'
  [                                  primStars 0 1 # lasting 1
  , (ramp # stretch 4) <#> \t  -> primStars t 1
  ,                                  primStars 1 1 # lasting 1
  , (ramp # stretch 2
             # backwards) <#> \so -> primStars 1 so
  ,                                  primStars 1 0 # lasting 1

  , keyframe ramp (primStars 1 0) orchard
      # stretch 2
  , orchard # lasting 1
  ]
  <∩>
  always (boundingRect (primStars 0 1) # scale 1.3 # fc white # lw none)
  where
    orchard = allprimdots2 (map rybColor [0..23]) [1..24]

smoothies :: ActF (Diagram B)
smoothies = stack
  [ cosRamp  <#> \t -> circle 0.1 # fc black # translateX (2*t - 1)
  , ramp     <#> \t -> circle 0.1 # fc black # translateX (2*fromRational t - 1) # translateY (-1)
  ]
  <∩>
  always (square 3 # fc white)

main :: IO ()
main = uniformGifMain 3 (samples 30 ((smoothies ->> backwards smoothies) # stretch 2))
-- main = uniformGifMain 3 (samples 30 anim)
-- main = mainWith primStars

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

movie' :: forall a. [ActF a] -> ActF a
movie' scenes = coerce (movie (coerce scenes :: [ActF (Last a)]))

keyframe
  :: ( Additive v, Metric v, Floating n, Ord n, Semigroup m
     , Real d
     )
  => ActF d -> QDiagram b v n m -> QDiagram b v n m -> ActF (QDiagram b v n m)
keyframe int d1 d2 = f <$> int
  where
    -- pairs :: [(Name, (Subdiagram b v n m, Subdiagram b v n m))]
    pairs = M.assocs $ M.intersectionWith (,) (getSubs d1) (getSubs d2)
    getSubs = M.map head . view (subMap . _Wrapped')

    -- f :: d -> QDiagram b v n m
    f d = mconcat
        . map (\(n,(s1,s2)) -> getSub s1 # translate (lerp (1 - d') zero (location s2 .-. location s1)))
        $ pairs
      where
        d' = realToFrac d
