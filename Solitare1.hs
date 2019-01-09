module Solitare1 where
    import System.Random
    import Data.List
    import Data.Maybe
    import Data.Function
    import Data.Ord
    import Debug.Trace
	--type of all the thing that needed in the assignment
    data Suit = Hearts | Diamonds | Spades | Clubs 
		deriving (Show, Ord, Eq, Enum)
    data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King  
		deriving (Show, Ord, Eq, Enum)

    type Card = (Pip, Suit)
    type Deck = [Card]

    -- Foundations and Columns are a stack, first item being the top
    type Foundations = [[Card]]
    type Columns = [[Card]]
    type Reserve = [Card]

    type EOBoard = (Foundations, Columns, Reserve)

    suits :: [Suit]
    suits = [Hearts, Diamonds, Spades, Clubs]

    suitList :: Suit -> [Card]
    suitList suit = [(pip, suit) | pip <- pipList]
    	where pipList = [Ace ..]
    pack :: Deck
    pack = suitList Clubs ++ suitList Diamonds ++ suitList Hearts ++ suitList Spades -- All cards of each suit

	--take a card and return its successor in same suit unless it's a King
	--when is a King, return a Queen for further use
    sCard :: Card -> Card
    sCard (pip, suit) = (succ pip, suit)

	--take a card and return its previous card in same suit unless it's an Ace
	--when is a Ace, error
    pCard :: Card -> Card
    pCard (pip, suit) = (pred pip, suit)

    isAce :: Card -> Bool
    isAce (pip, _) = pip == Ace
    
    isQueen :: Card -> Bool
    isQueen (pip,_) = pip == Queen

    isKing :: Card -> Bool
    isKing (pip, _) = pip == King

    -- Shuffle returns the full 52 card deck in a random order depending on the given seed
    shuffle :: Int -> Deck
    shuffle seed =
        let 
        	sortedPark = sortBy (comparing snd) (zip cards randomNumList)

        	rng = mkStdGen seed
        	cards = pack
        	randomNumList = take 52 (randoms rng::[Int])
        in map fst sortedPark

    -- eoDeal creates 8 columns of 6 cards, does this by using split on the pack
    -- takes a seed for the random pack
    eODeal :: Int -> EOBoard
    eODeal seed = 
        let shuffledDeck = shuffle seed 
			--shuffle the list of cards
            reserves = take 4 shuffledDeck 
			-- take four out for the reserves
            foundations = [] 
			-- foundations with 0 cards
            columns = splitDeck (drop 4 shuffledDeck) 
			-- Split the remaining cards into 8 columns of 6
        in (foundations,columns,reserves)
    	
    splitDeck :: Deck -> [Deck]
    splitDeck [] = []
    splitDeck deck = h : splitDeck t
    	where (h,t) = splitAt 6 deck
    
    	
    -- take a board and a card adn return a board to teh card to teh  foundation
    makemove :: EOBoard -> Card -> EOBoard
    makemove (founds, cols, res) card@(pip,_)
    --if ace go to the foundation and do nothing else
        | pip == Ace = ([card]:founds, filter (not.null) (foldl (updateCols card) [] cols), filter (/= card) res)
        | otherwise = (map (updateFounds card) founds, filter (not.null) (foldl (updateCols card) [] cols), filter (/= card) res)
        where
            -- from the column removes the card from the head of the column
            updateCols card acc [] = acc
            updateCols card acc (x:xs)
                | card == x = xs:acc
                | otherwise = (x:xs):acc
            -- from teh  foundaiton puts the card on the top of the foundation
            updateFounds card (x:xs)
                | isKing x = x:xs
                | card == sCard x = card:x:xs
                | otherwise = x:xs
    --select the  head of the each column if any card can be choose 
    selectHeads :: [[a]] -> [a]
    selectHeads lists = [head list| list <- lists, (not . null) list]
    
    -- Gets the top card from the columns and all the reserve cards
    getPlayableCards :: EOBoard -> [Card]
    getPlayableCards (_, cols, res) = selectHeads cols ++ res

    -- get the successor over the list and get the individual one
    getFoundationSuccs :: Foundations -> [Card]
    getFoundationSuccs = foldl getFoundSucc []
        where
            -- getFoundSucc gets the successor from a foundation
            getFoundSucc acc (x:xs)
                | isKing x = acc
                | otherwise = sCard x:acc

    -- The movableCards are the top cards that are either the successors
    -- to the foundations or aces that can be move to foundation
    getMovableCards :: EOBoard -> [Card]
    getMovableCards board@(f, c, r) =
        let playableCards = getPlayableCards board
            foundationSuccs = getFoundationSuccs f
        in filter (\x -> x `elem` foundationSuccs || isAce x) playableCards

	--final function that calls helperToFunction to make the move until no move can be made
    toFoundations :: EOBoard -> EOBoard
    toFoundations board@(founds, _, _)
        | null movableCards = board -- No movableCards so return the board
        -- Recursively call the function  with the board after making the possible moves
        | otherwise = toFoundations  (foldl makemove board movableCards)
        where
            movableCards = getMovableCards board
    
    --function that remove the card from a list of list
    remove :: Eq a => a -> [[a]] -> [[a]]
    remove a [] = []
    remove a (x:xs) 
   		| elem a x = (delete a x : xs)
   		| otherwise = (x : remove a xs)