import Test.HUnit

import qualified Data.Set as Set
import Data.Ix (range)

import Life

main = runTestTT tests

findTests prefix = filter checkName $ getTests tests
  where checkName (TestLabel n _) = take (length prefix) n == prefix
        getTests (TestList l) = l

run = runTestTT . TestList . findTests

-- Helper methods:

nth_next_board 0 board = board
nth_next_board n board = nth_next_board (n-1) (next_board board)

coords = Set.fromList

-- Tests:

tests = test [

  "cell dies if underpopulated" ~: 
    False @=? any stays_alive [0, 1]
  ,
  "cell dies if crowded" ~: 
    False @=? any stays_alive (range (4,8))
  ,
  "cell stays alive if correctly populated" ~:
    True @=? all stays_alive [2, 3]
  ,

  "cell stays dead if underpopulated" ~: 
    False @=? any becomes_alive [0, 1, 2]
  ,
  "cell stays dead if crowded" ~: 
    False @=? any becomes_alive (range (4,8))
  ,
  "cell becomes alive if correctly populated" ~:
    True @=? becomes_alive 3
  ,

  "calculates neighbouring coordinates" ~:
    neighbour_coords (3,7) @?= coords [
      (2,6), (3,6), (4,6),
      (2,7),        (4,7),
      (2,8), (3,8), (4,8)
    ]
  ,
  "calculates dead cells around living cells" ~:
    dead_cells_around (coords [(3,7),(4,8)]) @?= coords [
      (2,6), (3,6), (4,6),
      (2,7),        (4,7), (5,7),
      (2,8), (3,8),        (5,8),
             (3,9), (4,9), (5,9)
    ]
  ,
  "calculates living neighbour count around cell" ~:
    2 @=? living_count_around (3,3) (coords [(2,3), (3,3), (3,4), (5,5)])
  ,

  "calculates next board for blinker" ~:
    next_board (coords [(4,5), (5,5), (6,5)]) @?= coords [(5,4), (5,5), (5,6)]
  ,
  "calculates next translation for glider" ~:
    nth_next_board 4 (coords [
             (2,1),
                    (3,2),
      (1,3), (2,3), (3,3)
    ]) @?= coords [
             (3,2),
                    (4,3),
      (2,4), (3,4), (4,4)
    ]

  ]
