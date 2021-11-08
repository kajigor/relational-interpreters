data Peano = Z | S Peano 
  deriving Eq

lt :: Peano -> Peano -> Bool 
lt Z (S _) = True 
lt (S _) Z = False 
lt (S x) (S y) = lt x y 
lt Z Z = False 

fromPeano :: Peano -> Int 
fromPeano Z = 0 
fromPeano (S x) = 1 + fromPeano x 

toPeano :: Int -> Peano 
toPeano 0 = Z 
toPeano x | x > 0 = S (toPeano (x - 1))
toPeano x | x < 0 = unknown 

minmax :: (Peano, Peano) -> (Peano, Peano)
minmax (x, y) = if lt x y then (x, y) else (y, x)

minmaxInt :: (Int, Int) -> (Int, Int) 
minmaxInt (x, y) = (fromPeano x', fromPeano y')
  where (x', y') = minmax (toPeano x, toPeano y)
 
