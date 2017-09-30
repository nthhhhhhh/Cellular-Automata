{- Hello, this is a cellular automata exercise. The algorithm was learned
    from homeboi Shiffman. He wrote an accessible book called the
    Nature of Code. You can find it here: natureofcode.com -}

module CellularAutomata (newGen, Generation, ruleFrom) where

import           Data.Sequence as S
import           Prelude       as P




type Rule = Seq Int
type Generation = Seq Int


-- make sure list has only 0s or 1s
shrekForNonBinary :: Seq Int -> Bool
shrekForNonBinary rule =
  not $ all (\x -> x == 0 || x == 1) rule


-- create an list of length 8
-- with binary values from an Integer
-- ranges from 1 to 255
ruleFrom' :: Int -> Rule
ruleFrom' 0   = S.empty
ruleFrom' num = ruleFrom' (num `div` 2) |> (num `mod` 2)

ruleFrom :: Int -> Rule
ruleFrom num
  | num > 255 || num < 1  = error "Cellular automata rules are between 1 and 255. At least these ones."
  | otherwise = result
    where
        rule = ruleFrom' num
        noZerosNeeded = 8 - S.length rule
        result =
          if noZerosNeeded > 0 then
              S.replicate noZerosNeeded 0 >< rule
          else
              rule

-- for a given rule, get the appropriate value for a triple
ruleToBinVal' :: Seq Int -> Rule -> Int
ruleToBinVal' triple rule
    | triple == S.fromList [1, 1, 1] = S.index rule 0
    | triple == S.fromList [1, 1, 0] = S.index rule 1
    | triple == S.fromList [1, 0, 1] = S.index rule 2
    | triple == S.fromList [1, 0, 0] = S.index rule 3
    | triple == S.fromList [0, 1, 1] = S.index rule 4
    | triple == S.fromList [0, 1, 0] = S.index rule 5
    | triple == S.fromList [0, 0, 1] = S.index rule 6
    | otherwise = S.index rule 7


ruleToBinVal :: Seq Int -> Rule -> Int
ruleToBinVal triple rule
    | P.length triple /= 3 = error "triple must be of length 3"
    | shrekForNonBinary (triple >< rule) = error "Rule or triple must have binary values."
    | otherwise = ruleToBinVal' triple rule


-- produces a the next generation of a generation
newGen :: Int -> Generation -> Generation
newGen rule seedGen =
  newGen' rule 0 seedGen S.empty


newGen' :: Int -> Int -> Generation -> Generation -> Generation
newGen' ruleAsInt pos seedGen finalGen
    | pos ==  S.length seedGen = finalGen        -- base: on out of bounds
    | pos == 0 =                                 -- on first elem
      newGen'
          ruleAsInt
          (succ pos)
          seedGen
          (finalGen |> ruleToBinVal firstCase rule)
    | pos == pred (S.length seedGen) =           -- on last elem
      newGen'
          ruleAsInt
          (succ pos)
          seedGen
          (finalGen |> ruleToBinVal lastCase rule)
    | otherwise =
      newGen'
          ruleAsInt
          (succ pos)
          seedGen
          (finalGen |> ruleToBinVal getNeighborhood rule )

      where
          seedGenAt = S.index seedGen
          getNeighborhood = S.fromList [seedGenAt (pred pos) -- the center and left right neighbors
                            , seedGenAt pos
                            , seedGenAt (succ pos)]
          rule = ruleFrom ruleAsInt
          firstCase = S.fromList [0, seedGenAt 0, seedGenAt 1]
          lastCase  = S.fromList [seedGenAt (S.length seedGen - 2)
                                 ,seedGenAt (S.length seedGen - 1)
                                 ,0]

