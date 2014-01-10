module Graphics.Math where

import           Data.List  ( intercalate )
import           Data.Maybe ( fromJust, fromMaybe )
import           Prelude hiding ( subtract )
import qualified Data.List as L

-- Vector Types

type Vector a = [a]

type Vec2 a = (a, a)
type Vec3 a = (a, a, a)
type Vec4 a = (a, a, a, a)

-- Vector functions

-- | Computes the magnitude.
magnitude :: Floating a => Vector a -> a
magnitude = sqrt . sum . map (**2)

-- | Computes the unit vector.
normalize :: Floating a => [a] -> [a]
normalize vec = map (/mag) vec
    where mag = magnitude vec

-- | Computes the unit vector.
unitize :: Floating a => [a] -> [a]
unitize = normalize

-- | Adds two vectors.
add :: Floating a => [a] -> [a] -> [a]
add = zipWith (+)

-- | Subtracts two vectors.

subtract :: Num a => [a] -> [a] -> [a]
subtract = zipWith (-)

type Matrix a = [Vector a]

-- Projection Matrices

orthoMatrix :: (Num t, Fractional t) => t -> t -> t -> t -> t -> t -> Matrix t
orthoMatrix left right top bottom near far = [ [ 2/(right-left), 0, 0, -(right+left)/(right-left) ]
                                             , [ 0, 2/(top-bottom), 0, -(top+bottom)/(top-bottom) ]
                                             , [ 0, 0, -2/(far-near), -(far+near)/(far-near) ]
                                             , [ 0, 0, 0, 1]
                                             ]


-- Affine Transformation Matrices

scaleMatrix3d :: Num t => t -> t -> t -> Matrix t
scaleMatrix3d x y z = [ [x, 0, 0, 0]
                      , [0, y, 0, 0]
                      , [0, 0, z, 0]
                      , [0, 0, 0, 1]
                      ]

rotationMatrix3d :: Floating t => t -> t -> t -> Matrix t
rotationMatrix3d x y z = [ [cy*cz, -cx*sz+sx*sy*sz,  sx*sz+cx*sy*cz, 0]
                         , [cy*sz,  cx*cz+sx*sy*sz, -sx*cz+cx*sy*sz, 0]
                         , [  -sy,           sx*cy,           cx*cy, 0]
                         , [    0,               0,               0, 1]
                         ]
    where [cx, cy, cz] = map cos [x, y, z]
          [sx, sy, sz] = map sin [x, y, z]

translationMatrix3d :: Num t => t -> t -> t -> Matrix t
translationMatrix3d x y z = [ [1, 0, 0, x]
                            , [0, 1, 0, y]
                            , [0, 0, 1, z]
                            , [0, 0, 0, 1]
                            ]

-- Basic Matrix Math

fromVector :: Int -> Int -> [a] -> Maybe [[a]]
fromVector r c v = if length v `mod` r*c == 0
                   then Just $ groupByRowsOf c v
                   else Nothing

-- | The identity of an NxN matrix.
identityN :: (Num a) => Int -> Matrix a
identityN n = groupByRowsOf n $ modList n
    where modList l = [ if x `mod` (l+1) == 0 then 1 else 0 | x <- [0..(l*l)-1] ]

-- | The identity of the given matrix.
identity :: (Num a) => Matrix a -> Matrix a
identity m = groupByRowsOf rows modList
    where modList = [ if x `mod` (cols+1) == 0 then 1 else 0 | x <- [0..len-1] ]
          len     = sum $ map length m
          rows    = numRows m
          cols    = numColumns m
-- | The number of columns in the matrix.
numColumns :: Matrix a -> Int
numColumns = length
-- | The number of rows in the matrix.
numRows :: Matrix a -> Int
numRows []    = 0
numRows (r:_) = length r
-- | A list of the columns.
toColumns :: Matrix a -> [[a]]
toColumns = transpose
-- | A list of the rows.
toRows :: Matrix a -> [[a]]
toRows = id
-- | The minor for an element of `a` at the given row and column.
minorAt :: Floating a => [Vector a] -> Int -> Int -> a
minorAt m x y = let del = deleteColRow m x y
                in determinant del
-- | The Matrix created by deleting column x and row y of the given matrix.
deleteColRow :: Matrix a -> Int -> Int -> Matrix a
deleteColRow m x y = let nRws = numRows m
                         nCls = numColumns m
                         rNdxs = [ row + x      | row <- [0,nCls..nCls*(nCls-1)] ]
                         cNdxs = [ nRws*y + col | col <- [0..nRws-1] ]
                         ndxs = rNdxs ++ cNdxs
                         (_, vec) = foldl filtNdx (0,[]) $ concat m
                         filtNdx (i, acc) el = if i `elem` ndxs
                                               then (i+1, acc)
                                               else (i+1, acc++[el])
                     in groupByRowsOf (nRws-1) vec
-- | The transpose of the matrix.
transpose :: Matrix a -> Matrix a
transpose = L.transpose

-- | Computes the inverse of the matrix.
inverse :: (Num a, Eq a, Fractional a, Floating a) => Matrix a -> Maybe (Matrix a)
inverse m = let det      = determinant m
                one_det  = 1/ det
                cofacts  = cofactors m
                adjoint  = transpose cofacts
                inv      = (map . map) (*one_det) adjoint
            in if det /= 0
               then Just inv
               else Nothing

-- | The matrix of cofactors of the given matrix.
cofactors :: (Num a, Floating a) => Matrix a -> Matrix a
cofactors m = fromJust $ fromVector (numRows m) (numColumns m) [ cofactorAt m x y | y <- [0..numRows m -1], x <- [0..numColumns m -1] ]

-- | Computes the multiplication of two matrices.
multiply :: (Num a, Show a) => Matrix a -> Matrix a -> Matrix a
multiply m1 m2 = let element row col = sum $ zipWith (*) row col
                     rows  = toRows m1
                     cols  = toColumns m2
                     nRows = numRows m1
                     nCols = numColumns m2
                     vec   = take (nRows*nCols) [ element r c | r <- rows, c <- cols ]
                     mM    = fromVector nRows nCols vec
                     err   = error $ intercalate "\n" [ "Could not multiply matrices:"
                                                      , "m1:"
                                                      , show m1
                                                      , "m2:"
                                                      , show m2
                                                      , "from vector:"
                                                      , show vec
                                                      ]
                 in fromMaybe err mM
-- | The cofactor for an element of `a` at the given row and column.
cofactorAt :: (Num a, Floating a) => Matrix a  -> Int -> Int -> a
cofactorAt m x y = let pow = fromIntegral $ x + y + 2 -- I think zero indexed.
                   in (-1)**pow * minorAt m x y

-- | Computes the determinant of the matrix.
determinant :: (Num a, Floating a) => Matrix a -> a
determinant [[a]] = a
determinant m  = let rowCofactors = [ cofactorAt m x 0 | x <- [0..numColumns m -1] ]
                     row = head $ toRows m
                 in sum $ zipWith (*) rowCofactors row

-- Helpers
groupByRowsOf :: Int -> [a] -> [[a]]
groupByRowsOf _    [] = []
groupByRowsOf cols xs = take cols xs : groupByRowsOf cols (drop cols xs)

