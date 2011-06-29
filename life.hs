module Life (
  Coord,
  CoordSet,

  stays_alive,
  becomes_alive,

  neighbour_coords,
  living_count_around,
  dead_cells_around,
  next_board
) where

import Data.Set as Set

type Coord = (Int,Int)
type CoordSet = Set Coord

stays_alive n_count = n_count >= 2 && n_count <= 3

becomes_alive = (==) 3

neighbour_coords :: Coord -> CoordSet
neighbour_coords (x,y) = fromList [
  (x-1,y-1), (x,y-1), (x+1,y-1),
  (x-1,y  ),          (x+1,y  ),
  (x-1,y+1), (x,y+1), (x+1,y+1)
  ]

living_count_around :: Coord -> CoordSet -> Int
living_count_around coord = size . intersection (neighbour_coords coord)

dead_cells_around :: CoordSet -> CoordSet
dead_cells_around living = around living `difference` living
  where around = fold union empty . Set.map neighbour_coords

next_board :: CoordSet -> CoordSet
next_board living = steady_life `union` new_life
  where
    steady_life = Set.filter (stays_alive   . count) living
    new_life    = Set.filter (becomes_alive . count) $ dead_cells_around living
    count coord = living_count_around coord living
