module Lib
  ( dividedDifferences
  , newtonInterpolant
  ) where

-- computes newton's divided differences for some function `f`
-- and a list of input values `xs`
dividedDifferences :: (Double -> Double) -> [Double] -> Double
dividedDifferences f [x] = f x
dividedDifferences f xs =
  let numerator = dividedDifferences f (init xs) - dividedDifferences f (tail xs)
      denominator = head xs - last xs
  in numerator / denominator


-- computes the value of the newton interpolant at `x` for some function `f`
-- and a list of interpolation points `xs`
newtonInterpolant :: (Double -> Double) -> [Double] -> Double -> Double
newtonInterpolant f xs x =
  let -- `sublists` is a list of sublists of xs. so given interpolation values
      -- [0, 1, 2], sublists is [[0], [0, 1], [0, 1, 2]]
      -- these sublists are used in both the following computations
      sublists = [take n xs | n <- [1..(length xs)]]

      -- the coefficients of the polynomial we're forming are the divided differences
      -- f[x1], f[x1,x2], f[x1,x2,x3], ...
      -- note that the arguments to each of the coefficients c1, c2, ... is simply
      -- the corresponding sublist
      coeffs = map (dividedDifferences f) sublists

      -- the newton is defined as f[x1] + f[x1,x2](x - x1) + f[x1,x2,x3](x - x1)(x - x2)
      -- this xChain computation is the list [1, (x - x1), (x - x1)(x - x2)]
      -- and is what we multiply the divided differences by in the polynomial
      xChain = 1 : [product [x - a | a <- sublist] | sublist <- init sublists]

  -- so overall, we have two lists of things that need to be multiplied at each index:
  -- coeffs: [f[x1], f[x1,x2], f[x1,x2,x3], ...]
  -- xChain: [1, (x - x1), (x - x1)(x - x2), ...]
  -- zipping these two lists, multiplying corresponding indices, and summing the resulting list
  -- will yield the newton interpolant polynomial
  in sum $ zipWith (*) coeffs xChain