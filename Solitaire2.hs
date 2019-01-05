module Solitaire2 where
import System.Random
import Solitaire1
import Data.Maybe
import Data.List
        
----------------------------------------------------------------------------------------------------------------------
--move King Card from Head of Columns to Empty Column
--checks the length of the column as well so that there is an empty column
--if there is no king cards in the column heads return the board
colKingToEmptyCol :: EOBoard -> EOBoard
colKingToEmptyCol board@(foundations,[],reserves) = board 
colKingToEmptyCol board@(foundations,columns,reserves)
    |( (length columns) < 8) &&  ( (length kingCardsList) >0)= (foundations,kingInNewColumn,reserves)
    |otherwise = board
        where
            --get a list of the kings in the heads of columns
            kingCardsList= filter (isKing) (map (head) (filter (\col -> length col>1) (columns)))      
            --king card would be the head of the king cards list
            kingCard = head (kingCardsList)                
            --insert the king card to the updated columns
            kingInNewColumn= [kingCard]: (map (delete kingCard) columns)
----------------------------------------------------------------------------------------------------------------------    
--move King Card from Reserves to Empty Column
--if any reserve card is king and there is an empty space in columns, move the king to the empty column   
--when there is a king card in reserves it puts it in the columns if there is an empty column
resKingToEmptyCol :: EOBoard -> EOBoard
resKingToEmptyCol board@(foundations,columns,[]) = board
resKingToEmptyCol board@(foundations,columns,reserves)
     |(length kingCardsList >0)  && (length columns < 8) = (foundations,([kingCard]:columns), delete kingCard reserves)
     |otherwise = board
     where
        --takes all the kings from the reserves
        kingCardsList= filter (isKing) reserves
        --takes the head of the list of kings in the reserves
        kingCard = head (kingCardsList)
        
----------------------------------------------------------------------------------------------------------------------        
--move Cards from Reserves to Columns
--if there is a card in reserves that is a pCard of a column head card 
--the card is moved to columns 
reserveToColumns :: EOBoard -> EOBoard
reserveToColumns board@(foundations,columns,[]) = board
reserveToColumns board@(foundations,columns,reserves)  
    --updates columns and reserves
    |(length intersection)>0 = (foundations,newColumns,delete card reserves)
    |otherwise = board   
    where
        --first find all the predecessors of the column heads
        pCardsOfColHeads = map (pCard) (filter (not.isAce) (columnHeadCards columns)) 
        --check if there is common cards between the predecessors of the column heads
        --and the cards in the reserves
        intersection=(intersect  pCardsOfColHeads reserves) --filter xoris ta prostateumena
        --the card to be moved is the head of the intersection
        card=head intersection
        --updates columns checks whether the column head is the successor of the card
        --and inserts it in that column
        newColumns = filter (not.null) (map (\col -> if (head col == sCard card) then card:col else col)
                                (filter (not.null) columns))      
----------------------------------------------------------------------------------------------------------------------
--this function takes the number of cards above the head of each column as a parameter
--and checks whether that card can go to foundations
--if the nth card of a column favours foundations
--move card to reserves
--for example if the integer is 1, it checks 1 card above the heads of columns if it favours foundations
nthColCardFavoursFoundations :: EOBoard -> Int -> EOBoard
nthColCardFavoursFoundations board@(foundations,columns,reserves) num
    --if the number above the heads is 0 the given board is returned
    --because there is no point as it will be the same as moving a column head to the reserve
    |(columns==[] || num==0) = board
    |( (length reserves) < 8) && ( (length intersection) > 0) = (foundations, (map (delete card) columns), (card:reserves))
    |otherwise = board
    where
        --finds all the cards that go to foundations
        --includes aces and successor cards of foundations cards that are not kings
        cardsForFoundations = (filter (isAce) pack) ++ (map (sCard) (filter (\card -> (not.isKing) card) (foundations)))
        --finds the nth cards above all the column heads that can be moved
        --and finds the common cards with the cards that match the foundation cards
        intersection=intersect (cardsForFoundations) (nthCardsAboveColumnHead num columns)
        --future move card is the card that is going to be moved in the future which is the head of 
        --the intersection above
        futureMoveCard= head intersection
        --take the first card of the column that contains the future move card
        card=(head (filter (\col -> (col!!num)==futureMoveCard) (filter (\col -> (length col>num) ) columns)))!!0     
        
----------------------------------------------------------------------------------------------------------------------
--if there are two cards in columns that match, move the predecessor to the successor card in the column
columnCardToCols :: EOBoard -> EOBoard
columnCardToCols board@(foundations,[],reserves) = board
columnCardToCols board@(foundations,columns,reserves)
    |(length intersection)>0 = (foundations,newColumns,reserves)
    |otherwise = board
    where
        --finds all the predecessors of the column head cards
        pCardsOfColHeads = map (pCard) (filter (not.isAce) (columnHeadCards columns))     
        --find the common cards between the predecessors of column head cards and the column head cards
        intersection=(intersect  pCardsOfColHeads (columnHeadCards columns) )
        card=head intersection
        --update columns by deleting the card and putting the card in the columns that its successor is
        newColumns = (filter (not.null) 
                          (map (\col -> if (head col == card) then delete card col
                            else if (head col == sCard card) then card:col else col)
                                (filter (not.null) columns)))
----------------------------------------------------------------------------------------------------------------------
--puts columns cards to reserves
columnCardToRes :: EOBoard -> EOBoard
columnCardToRes board@(foundations,[],reserves) = board
columnCardToRes board@(foundations,columns,reserves)
   |(length reserves < 8) && ((length availableColHeads) > 0 )=  (foundations, (map (delete card) columns), (card:reserves))
   |otherwise = board
   where
        --find the head column cards that can be moved
        availableColHeads= nthCardsAboveColumnHead 0 columns        
        card = head availableColHeads                    
----------------------------------------------------------------------------------------------------------------------
--finds the nth card above the column head
nthCardsAboveColumnHead :: Int -> Columns -> Deck
nthCardsAboveColumnHead num columns
    --if num is 0 it means that it looks for the first card of the column that is not a king and also checks 
    --that the second card of the column is not a successor of the first card
    |(num==0) = map head ((filter (\col -> (length col>1) && (not(isKing (col!!0)) ) && 
                                (sCard(col!!0)/=(col!!1))) (columns)))
    --returns the card needed by checking the length of its column as well 
    |otherwise = map (!!num) (filter (\col -> if (length col>1) then (length col)>(num) && 
                    not(cardsMatch (col!!0) (col!!1)) else (length col)>(num)) columns)
                
 ---------------------------------------------------------------------------------------------------------------------   
    
--all head cards except aces because they have no pCard
columnHeadCards :: Columns -> Deck
columnHeadCards columns = (map head (filter (not.null)columns)) 
------------------------------------------------------------------
--checks if the successor of the first card is the second card
--check for kings as well; if the first card supplied is a king 
--it returns false
cardsMatch :: Card -> Card -> Bool
cardsMatch card1 card2 
    |(not.isKing) card1 = (sCard card1)==card2
    |otherwise = not(isKing card1) --which is false
----------------------------------------------------------------------------------------------------------------------
--this function takes the number of cards above the head of each column as a parameter
--and checks whether that card can go to another column head
--move the head card of the column that contains the nth card, to reserves
--for example if the integer is 1, it checks 1 card above the heads of columns if it is a predecessor of a column head
nthColCardToResColumns :: EOBoard -> Int -> EOBoard
nthColCardToResColumns board@(foundations,columns,reserves) num
    |(columns==[] || num==0) = board
    |( (length reserves) < 8) && ( (length intersection) > 0) = (foundations, (map (delete card) columns), (card:reserves))
    |otherwise = board
    where
        --finds all the cards that go to other column heads
        --includes aces and successor cards of foundations cards that are not kings
        cardsForColumns = (map (pCard) (filter (\card -> (not.isAce) card) (columnHeadCards columns)))       
        intersection=(intersect (cardsForColumns) (nthCardsAboveColumnHead num columns))
        --gives priorities to queens
        futureMoveCard= head intersection
        card= (head (filter (\col -> (col!!num)==futureMoveCard) (filter (\col -> (length col>num) ) columns)))!!0

              
board1 = ([(Ace,Spades)],[[(Seven,Hearts)],[(Six,Hearts)]],[(Four,Hearts),(Ace,Diamonds)])
----------------------------------------------------------------------------------------------------------------------
freeUpKingCard:: EOBoard -> EOBoard
freeUpKingCard board@(foundations,columns,reserves)
    |(length (availableCardList) >0) && (length reserves <8) = (foundations,(map (delete availableCard) columns),(availableCard:reserves))
    |otherwise = board
        where
        --find the cards that have kings that are 1 place above the head of each column and that the head of the column
        --is not a predecessor of a king
        availableCardList= map (!!0) (filter (\col -> (length col==2) && (isKing (col!!1)) && (col!!0 /=pCard (col!!1))  ) (columns))
        availableCard=head availableCardList
---------------------------------------------------------------------------------------------------------------------- 
-- Maybe helper                
resMaybe :: (Maybe a) -> a
resMaybe (Just x) = x 

------------------------------------------------------------------
 -- find possible moves
findMoves :: EOBoard -> [EOBoard]
findMoves board= 
        filter(\st->st/=board) --moves listed by priority
            ( [toFoundations board] ++
                --move king from column head to empty column
                [colKingToEmptyCol board] ++ 
                --move king from reserves to empty column
                [resKingToEmptyCol board] ++               
                --frees up king cards
                [freeUpKingCard board] ++
                --checks nth cards above the heads of columns if it favours foundations
                [nthColCardFavoursFoundations board 1]++ 
                [nthColCardFavoursFoundations board 2]++ 
                [nthColCardFavoursFoundations board 3]++               
                --column head card move to other column head card
                [columnCardToCols board]++
                --frees up king from a column
                [nthColCardToResColumns board 1]++ 
                [nthColCardToResColumns board 2]++
                [nthColCardToResColumns board 3]++
                [nthColCardToResColumns board 4]++
                --checks the fourth card from the head of columns if it goes to foundations
                [nthColCardFavoursFoundations board 4]++ 
                --makes a move from reserves to columns
                [reserveToColumns board] ++                                     
                --moves the first card of columns to reserves
                [columnCardToRes board] ) 
                               
-----------------------------------------------------------------
-- choose move
chooseMove :: EOBoard -> Maybe EOBoard
chooseMove board
    | null (findMoves board) = Nothing
    | otherwise = Just (head (findMoves board))
----------------------------------------------------------------- 
--Takes an EOBoard and returns an Int equal to the score after playing a game to completion            
eOGame :: EOBoard -> Int
eOGame board
    | isNothing (chooseMove board) = findScore board
    | otherwise = eOGame (resMaybe (chooseMove board) )
-----------------------------------------------------------------
--Takes a seed and the number of games to be played 
--Returns the number of wins and the average score as a tuple
--fromIntegral = convert from any Integral type into any Numeric type
eOExpt :: Int ->(Int,Float)
eOExpt seed = (winsNumber,avScore)
     where listBoards = map (eODeal) (seedList seed gamesPlayed) 
           listScores = map eOGame listBoards
           avScore = fromIntegral (sum listScores)/ fromIntegral gamesPlayed
           winsNumber = length (filter (==52) listScores)
           gamesPlayed = 100
------------------------------------------------------------------
seedList :: Int -> Int -> [Int]
seedList seed gamesPlayed= (take (gamesPlayed) ([seed..(seed+gamesPlayed)]) )
------------------------------------------------------------------
findScore :: EOBoard -> Int 
findScore (fnds,cols,res) = (52- (length res) - (foldr (+) 0 (map length cols)))
------------------------------------------------------------------

testBoard1 = ([],[[(King,Diamonds)],[(King,Hearts)],[(Eight,Clubs),(Nine,Clubs),(Ten,Clubs),(Six,Spades),(Jack,Spades),(Four,Hearts),(Eight,Hearts)],[(Nine,Diamonds),(Ten,Diamonds),(Jack,Diamonds),(Seven,Spades),(Seven,Clubs),(Jack,Clubs),(Queen,Clubs),(Eight,Diamonds)],[(Six,Hearts),(Seven,Hearts),(Two,Clubs),(King,Clubs),(Five,Spades)],[(Queen,Spades),(King,Spades),(Queen,Diamonds)],[(Three,Clubs),(Four,Clubs)],[(Eight,Spades),(Nine,Spades),(Six,Clubs),(Queen,Hearts)]],[])
