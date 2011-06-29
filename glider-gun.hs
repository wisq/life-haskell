import Life
import qualified Data.Set as Set
import System.Environment (getArgs)

start :: CoordSet
start = Set.fromList [
    (25,0),
    (22,1),(23,1),(24,1),(25,1),(30,1),
    (13,2),(21,2),(22,2),(23,2),(24,2),(30,2),
    (12,3),(14,3),(21,3),(24,3),(34,3),(35,3),
    (11,4),(15,4),(16,4),(21,4),(22,4),(23,4),(24,4),(34,4),(35,4),
    (0,5),(1,5),(11,5),(15,5),(16,5),(22,5),(23,5),(24,5),(25,5),
    (0,6),(1,6),(11,6),(15,6),(16,6),(25,6),
    (12,7),(14,7),
    (13,8)
  ]

count :: CoordSet -> String
count = show . Set.size

main :: IO ()
main = do
  args <- getArgs
  board_loop 1 (read $ head $ args ++ ["-1"]) start

board_loop :: Int -> Int -> CoordSet -> IO ()
board_loop _ 0 board = do
    putStrLn $ "Finished: " ++ (count board) ++ " living cells."

board_loop n remain board = do
    putStrLn $ "Iteration " ++ (show n) ++ ": " ++ stats
    board_loop (n+1) (remain-1) next
  where
    next  = next_board board
    stats = (count board) ++ " => " ++ (count next)
