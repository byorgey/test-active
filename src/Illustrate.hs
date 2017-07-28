{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

axes :: Diagram B
axes = mconcat
  [ hashes
  , hashes # rotateBy (1/4)
  , arrowV (5.5 *^ unitX)
  , arrowV (5.5 *^ unitY)
  ]
  # lw thick
  where
    hashes = atPoints (iterateN 5 (translateX 1) (1 ^& 0)) (repeat (vrule 0.15))

illustrateActiveSum :: Active f (Sum Double) -> Diagram B
illustrateActiveSum = illustrateActive . fmap getSum

illustrateActiveR :: Active f Rational -> Diagram B
illustrateActiveR = illustrateActive . fmap fromRational

illustrateActive :: Active f Double -> Diagram B
illustrateActive = illustrateActive' (1/2) []

type Discontinuity = (Rational, DiscontinuityType)
data DiscontinuityType = OC | CO | CC | N

illustrateActive' :: Rational -> [Discontinuity] -> Active f Double -> Diagram B
illustrateActive' pd discs = frame 0.5 . withActive (endPt <> base) base
  where
    endPt act
      = closedPt
        # moveTo (fromRational (durationF act) ^& end act)
    base :: Active f Double -> Diagram B
    base act = foldMap drawSegment segments
      where
        portionToIllustrate = act # cut 5.5
        discs' = (0,OC) : discs ++ [(durationF portionToIllustrate, N)]
        segments = zip discs' (tail discs')
        drawSegment ((s,d1), (e,d2)) = mconcat [endpts, spline, axes]
          where
            eps = 1/100
            s' = case d1 of { CO -> s + eps; _ -> s }
            e' = case d2 of { OC -> e - eps; _ -> e }
            act' = slice s' e' act
            spline =
              ( zipWith (^&) (map fromRational [s, s + pd ..]) (samples (1/pd) act')
                ++ [fromRational e ^& end act']
              )
              # cubicSpline False
              # lc red
            endpts = mconcat
              [ (case d1 of CO -> openPt ; _ -> closedPt)
                  # moveTo (fromRational s ^& start act')
              , (case d2 of N -> mempty ; OC -> openPt ; _ -> closedPt)
                  # moveTo (fromRational e ^& end act')
              ]

closedPt, openPt :: Diagram B
closedPt = circle 0.1 # lw none  # fc red
openPt   = circle 0.1 # lc red   # fc white

-- main = mainWith (illustrateActive' 0.1 [(2, OC)] (movie [2 # lasting 2, 3 # lasting 3]))
main = mainWith (illustrateActive (fromRational <$> ui))
