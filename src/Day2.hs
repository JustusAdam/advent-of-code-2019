#!/usr/bin/env stack
-- stack script --resolver=lts-14.16 --package vector
import Control.Monad (forever)
import Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Control.Monad.ST
import Text.Printf

type Tape = V.Vector Int

run :: Tape -> Either (Int, Int) Tape
run initialTape = runST $ do
    tape <- V.thaw initialTape

    let step position = do
            let get = MV.read tape
            op <- get position
            let doOp op = do
                    op1 <- get =<< get (position + 1)
                    op2 <- get =<< get (position + 2)
                    dest <- get (position + 3)
                    MV.write tape dest (op1 `op` op2)
                    step (position + 4)
            case op of
                99 -> Right <$> V.freeze tape
                1 -> doOp (+)
                2 -> doOp (*)
                op -> pure $ Left (op, position)
    step 0

parse = V.fromList . Prelude.read

interactive = forever $
    print . fmap V.toList . run . parse =<< getLine

getMachine =
    parse . ("[" ++) . (++"]") <$> readFile "input/day2.txt"

evalMachine =
    either
        (\(op, pos) ->
             error $ printf "Unknown operator %d at position %d" op pos)
        id .
    run

-- Interactive evaluator for puzzle two. input is the noun and the verb numbers
-- separated with a space
interactiveMachine = forever $ do
    machine <- getMachine
    [noun, verb] <- map Prelude.read . words <$> getLine
    print $ evalMachine (machine V.// [(1,noun), (2,verb)]) V.! 0

puzzle1 = do
    inp <- (V.// [(1,12),(2,2)]) <$> getMachine
    print $ evalMachine inp V.! 0

main = interactiveMachine
