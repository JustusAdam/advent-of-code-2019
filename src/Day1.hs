#!/usr/bin/env stack
-- stack script --resolver=lts-14.16
import Control.Monad (forever, (>=>))

calc :: Rational -> Integer
calc mass = floor (mass / 3) - 2

integrating mass | fuel <= 0 = 0
                 | otherwise = fuel + integrating fuel
  where
    fuel = calc $ fromIntegral mass

interactive = forever $
    print . integrating . read =<< getLine

fromFile = readFile >=> print . sum . map (integrating . read) . lines

main = fromFile "input/day1-puzzle1.txt"
