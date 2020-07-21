{-# LANGUAGE BangPatterns
           , DeriveDataTypeable
           , DeriveFoldable
           , DeriveFunctor
           , DeriveGeneric
           , DeriveTraversable
           #-}

module Linear.Geometry.Euclidean.V2 where

import Control.Applicative

import Control.DeepSeq

import Control.Monad

import Data.Data

import Data.Typeable

import qualified Data.Vector as V

import GHC.Exts (IsList)

import GHC.Generics

import qualified Linear as L

newtype Polygon a = Polygon { unPolygon :: V.Vector (L.V2 a) }
                  deriving ( Functor
                           , Foldable
                           , Traversable
                           , Eq
                           , Ord
                           , Read
                           , Show
                           , Data
                           , Typeable
                           , Generic
                           )

instance NFData a => NFData (Polygon a)

-- | A 'Polygon' must have at least three points to have any edges.
edges :: Polygon a -> [L.V2 (L.V2 a)]
edges (Polygon v)
    | V.length v < 3 = []
    | otherwise = go (V.unsafeHead v) v
    where go i vs
            | V.length vs == 1 = [L.V2 (V.head vs) i]
            | otherwise        = (L.V2 (V.head vs) (vs V.! 1))
                                 : go i (V.tail vs)

count :: (a -> Bool) -> [a] -> Int
count p = go 0
    where go !n [] = n
          go !n (x:xs) | p x       = go (n + 1) xs
                       | otherwise = go n xs

contains :: (Num a, Fractional a, Ord a) => Polygon a -> L.V2 a -> Bool
contains poly (L.V2 px py) =
    let vertRel (L.V2 (L.V2 _ e1y) (L.V2 _ e2y)) = (e1y > py) /= (e2y > py)
        doesCross (L.V2 (L.V2 ix iy) (L.V2 jx jy)) =
            (  ((iy > py) >= (jy > py))
            && (px < ((((jx - ix) * (py - iy)) / (jy - iy)) + ix))
            )
    in odd (count doesCross (filter vertRel (edges poly)))
