module Run where
    import EOIO
    import System.Random
    import Data.List
    import Data.Maybe
    import Data.Function
    import Data.Ord
    import Debug.Trace
    import Solitare1 
    import Solitare2

    
    eOGame :: EOBoard -> Int
    eOGame board@(foundation, columns, reserves)
        | sum (map length columns)==0 && length(reserves)==0 = 52
        | null move =  52 - length (reserves) - sum (map length columns) -- No available moves
        | otherwise = eOGame (fromJust move)
        where
            move = chooseMove board

    -- Given a starting seed, this plays 100 games using eOGame
    -- and eODeal with the starting seed, and computes how many wins
    -- and the average score for the 100 games
    eOExpt :: Int -> (Int, Float)
    eOExpt seed = let
        games = map (eOGame.eODeal)seedFinal
        wins = length (filter (== 52) games)
        averageScore = fromIntegral (sum games) / 100
        in (wins, averageScore)
			where seedFinal = [seed..(seed+99)]