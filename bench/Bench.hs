module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Vector.Primitive as VP

dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = Prelude.sum $ Prelude.zipWith (*) xs ys

dotProductMassiv ::
     (Num e, Source r1 ix e, Source r2 ix e)
  => Array r1 ix e
  -> Array r2 ix e
  -> e
dotProductMassiv xs ys = A.sum $ A.zipWith (*) xs ys

dotProductVector :: (Prim a, Num a) => VP.Vector a -> VP.Vector a -> a
dotProductVector xs ys = VP.sum $ VP.zipWith (*) xs ys

main :: IO ()
main = do
  let listX = [1 .. 1000000] :: [Float]
      listY = (* 5) <$> listX
      vecMaX = A.fromList Seq listX :: A.Vector P Float
      vecMaY = A.fromList Seq listY :: A.Vector P Float
      vecX = VP.fromList listX
      vecY = VP.fromList listY
  defaultMain
    [ bgroup
        "Bench"
        [ env (pure (listX, listY)) $ \ ~(xs, ys) ->
            bench "dotProduct (list)" $ whnf (dotProduct xs) ys
        , env (pure (vecMaX, vecMaY)) $ \ ~(xs, ys) ->
            bench "dotProduct (massiv - Seq)" $ whnf (dotProductMassiv xs) ys
        , env (pure (vecMaX, vecMaY)) $ \ ~(xs, ys) ->
            bench "dotProduct (massiv - Par)" $ whnf (setComp Par xs !.!) ys -- built-in dot
        , env (pure (vecX, vecY)) $ \ ~(xs, ys) ->
            bench "dotProduct (vector)" $ whnf (dotProductVector xs) ys
        ]
    ]
