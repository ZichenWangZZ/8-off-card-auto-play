module Solitare2 where
    import System.Random
    import Data.List
    import Data.Maybe
    import Data.Function
    import Data.Ord
    import Debug.Trace
    import Solitare1
    
    -- Gets all card that can move to column
    cardToColumnMove :: EOBoard -> [EOBoard]
    cardToColumnMove board = foldl (\acc x -> if cardToColumn board x /= board then cardToColumn board x:acc else acc) [] (getPlayableCards board)

    --if the card can move to the  column and with its successor, move it. andn delete it from the original place
    cardToColumn :: EOBoard -> Card -> EOBoard
    cardToColumn board@(foundation, column, reserve) card
        --we can start it with a king if there are less than 8 column
        | isKing card && length column < 8 = (foundation, [card]:foldl (getOtherCols card) [] cols, res)
        | not (isKing card || null moveCol) = (foundation, (card:head moveCol):foldl (getOtherCols card) [] cols, res)
        | otherwise = board--if nothing can be
        where
            cols = filter (not.null) column
            moveCol = filter (\(x:xs) -> sCard card == x) cols
            getOtherCols _ acc [] = acc
            getOtherCols card acc (x:xs)
                | x == card = xs:acc -- Remove moved card from the original column
                | isKing card || x /= sCard card = (x:xs):acc
                | otherwise = acc
            res = filter (/= card) reserve

    -- Gets all possible column head to reserve moves
    cardToReserveMove :: EOBoard -> [EOBoard]
    cardToReserveMove (foundation, column, reserves)
        | length reserves < 8 = foldl (\acc card -> (foundation, [if x == card then xs else x:xs | (x:xs) <- column], card:reserves):acc) [] (map head . filter (not.null) $ column)--can move if there are less than 8 card
        | otherwise = [] -- Moves can only happen if there is free space in the reserve

    -- All the possible moves from a board are res-to-col moves, col-to-col moves and col-to-res moves
    findMoves :: EOBoard -> [EOBoard]
    findMoves board = cardToColumnMove board ++ cardToReserveMove board

    -- Gets all possible single moves for a board after moves to foundations and chooses the best move


	--choose the best move at the depth n and the return of it will be the  best one when we evaluate it
    getBestSuccessor :: Int -> EOBoard -> (EOBoard, Int)
    getBestSuccessor 1 board = getBestBoard board (map (\b -> (b, evaluateBoard b)) position)
        where
            position = findMoves.toFoundations $ board
    getBestSuccessor depth board = getBestBoard board bestNext
        where
            -- Recurse with all the possible baords from the current position
            bestNext = foldl (\acc b -> getBestSuccessor (depth-1) b:acc) [] (position)
            position = findMoves.toFoundations $ board
			
    -- The best board is the board with the highest score
    getBestBoard :: EOBoard -> [(EOBoard, Int)] -> (EOBoard, Int)
    getBestBoard board boards
        | not (null boards) = highestBoard -- Gets the board with the highest score
        | otherwise = (board, 0)
        where highestBoard = maximumBy (comparing snd) boards-- If there are no moves then return the original board

    -- Takes a board and returns an integer representing how good the board is
	--take a board adn return the score of that board, good board will have high score
    evaluateBoard :: EOBoard -> Int
    evaluateBoard board@(foundation, column, reserve) = 
    	let
		-- the length of each part will be take point
        	foundCount = count1
        	toFoundCount = count2
        	inOrderCols = columnInOrder column
        	resCount = length reserve
	--foundation and col will have higher point adn the reserves will have less point return thses to let the move more earier it choose
        in (foundCount*30) + (toFoundCount*30) + (inOrderCols*30) + (resCount*10)
			where count1 = sum (map length foundation); --the card that in foundation
				  count2 =length (getMovableCards board);	--the card that can move 		
				  count3 = columnInOrder ; -- the card that in orderd column  
				  count4 = length reserve -- the card in reserve
				  
	--count have many card are in column are in order with the king in the botton and the cards are in order with the scard under it
    columnInOrder :: Columns -> Int
    columnInOrder = foldl (checkInOrder 0) 0
        where
            checkInOrder _ acc [] = acc
            checkInOrder number acc (x:xs)
                | isKing x && null xs = acc + number + 1
                | not (null xs || isKing x) && sCard x == head xs = checkInOrder (number+1) acc xs
                | otherwise = acc
    
	-- Gets all possible single moves for a board after moves to foundations and chooses the best move           
    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board@(foundation, column, reserve)
		| column == [] && reserve == [] = Nothing
        | boardScore /= evaluateBoard (fst best) && boardScore /= snd bestNext = Just (toFoundations (fst best))
        | otherwise = Nothing -- Board score is not increasing so choose no move in order to stop useless recursion
        where
            boardScore = evaluateBoard board
            best = getBestSuccessor 3 board -- Gets the best move from this position depending on board score in three moves. (four will take too much time)
            bestNext = getBestSuccessor 1 (fst best) -- it may cause infinite loops (res-col-res-col..) and just go further to the next successor

        
    