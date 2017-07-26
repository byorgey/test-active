{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Data.Coerce
import qualified Data.Map                            as M

import           Data.Colour.Palette.BrewerSet
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

theTriangle :: Path V2 Double
theTriangle = fromOffsets [3 *^ unit_Y, 4 *^ unitX] # closeTrail # toPath # alignB

theTriangles :: M.Map Name (Path V2 Double)
theTriangles = M.fromList $ zip (map toName [0 :: Int ..]) (zipWith rotateBy [0, 1/4 ..] (replicate 4 theTriangle))

drawNamedPaths :: Style V2 Double -> M.Map Name (Path V2 Double) -> [Diagram B]
drawNamedPaths s = map (\(n,p) -> stroke p # applyStyle s # named n) . M.assocs

triColor, sqColor :: Colour Double
triColor = brewerSet Paired 3 !! 1
sqColor  = brewerSet Paired 3 !! 0

pythagoreanSquarePath :: Path V2 Double
pythagoreanSquarePath
  = atPoints
      (square 7 # rotateBy (3/4))
      (M.elems theTriangles)
  # centerXY

pythagoreanSquare :: Diagram B
pythagoreanSquare
  = atPoints
      (square 7 # rotateBy (3/4))
      (drawNamedPaths (mempty # fc triColor) theTriangles)
  # centerXY

innerSquare :: Path V2 Double
innerSquare = pythagoreanSquarePath
  # fixPath
  # map (!!2)
  # reverse
  # unfixTrail
  # over located (glueTrail)
  # toPath

smallSquares :: Diagram B
smallSquares
  = mconcat
    [ (t1 <> t3 # alignBL) # alignTR
    , (t2 <> t4 # alignBR) # alignBL
    ]
    # centerXY
    # fc triColor
  where
    [t1,t2,t3,t4] = drawNamedPaths (mempty # fc triColor) theTriangles

otherSquares :: Diagram B
otherSquares
  = mconcat
    [ square 4 # alignBR
    , square 3 # alignTL
    ]
    # fc sqColor
    # centerXY

movie' :: forall a n. (Num n, Ord n) => [Active n F a] -> Active n F a
movie' scenes = coerce (movie (coerce scenes :: [Active n F (Last a)]))

delay
  :: (Monoid a, Semigroup a, Num d, Ord d)
  => d -> Active d f a -> Active d f a
delay d = (lasting d mempty ->-)

atDurations
  :: (Monoid a, Semigroup a, Num d, Ord d)
  => [(d, Active d F a)] -> Active d F a
atDurations = stack . map (uncurry delay)

infixr 0 ==>
(==>) :: a -> b -> (a,b)
(==>) = (,)

-- keyframe opts: some way(s) to specify paths can do even better than
-- just translating: we know about rotation etc.  get the SubMap
-- itself, each Subdiagram has DownAnnots

keyframe
  :: ( Additive v, Metric v, Floating n, Ord n, Semigroup m
     , Real d, Fractional d
     )
  => QDiagram b v n m -> QDiagram b v n m -> Active d F (QDiagram b v n m)
keyframe d1 d2 = f <$> ui
  where
    -- pairs :: [(Name, (Subdiagram b v n m, Subdiagram b v n m))]
    pairs = M.assocs $ M.intersectionWith (,) (getSubs d1) (getSubs d2)
    getSubs = M.map head . view (subMap . _Wrapped')

    -- f :: d -> QDiagram b v n m
    f d = mconcat
        . map (\(n,(s1,s2)) -> getSub s1 # translate (lerp (1 - d') zero (location s2 .-. location s1)))
        $ pairs
      where
        d' = fromRational . toRational $ d

anim :: Active Rational F (Diagram Rasterific)
anim = movie'
  [ always pythagoreanSquare
    <⊓>
    ( movie'
      [ const mempty # lasting 2
      , fadeIn                 1
      , id           # lasting 2
      , fadeOut                1
      , const mempty # lasting 1
      ]
      <:*> always (innerSquare # stroke # fc sqColor)
    )
  , always (square 7)
    <⊓>
    movie'
    [ kf     # stretch 2
    , always (end kf)
      <⊓>
      ( movie'
        [ const mempty # lasting 2
        , fadeIn                 1
        , id           # lasting 2
        , fadeOut                1
        , const mempty # lasting 1
        ]
        <:*> always otherSquares
      )
    , keyframe smallSquares pythagoreanSquare # stretch 2
    ]
  ]
  <⊓> always (square 8 # fc white)

  where
    kf = keyframe pythagoreanSquare smallSquares

main :: IO ()
main = uniformGifMain 3 (simulate 30 anim)


-- Need a better story in general for going between paths (which are
-- geometric and can be inspected, decomposed, etc.) and diagrams
-- (which can be given names and attributes).  If we just had a way to
-- extract the paths out of a diagram again that would be enough.  I
-- guess we'll get this with cchalmers' redesign.
