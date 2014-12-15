module MancalaBoard (MancalaBoard, Player, initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners) where

import Data.List as List -- for List.elemIndex
import Data.Maybe as Maybe -- for List.elemIndex

data MancalaBoard = MancalaBoardImpl { pits :: [Int], player :: Player }

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers

playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers

playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i

nextPlayer :: Player -> Player
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers

---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
    where boardSide = take boardSize (repeat startStones) ++ [0]

{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)

{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)

{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1

---- Retrieve information about Mancala Board ----

{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
getCurPlayer board = player board 

testGetCurPlayer :: Bool
testGetCurPlayer = PlayerA == getCurPlayer initial 

{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
getBoardData board = pits board

testGetBoardData :: Bool
testGetBoardData = take 14 (cycle (replicate 6 4 ++ [0])) == getBoardData initial 

{- return the side of the board for a specified player, including the store at
 - the end -}
playerSide :: MancalaBoard -> Player -> [Int]
playerSide board p = [ layout !! x | x <- (indicesForPlayerSide p)++[indexForPlayerStore p] ]
	where layout = getBoardData board

testPlayerSide :: Bool
testPlayerSide = (replicate 6 4 ++ [0]) == playerSide initial PlayerA

{- return the number of captured pieces in specified player's store -}
numCaptured :: MancalaBoard -> Player -> Int 
numCaptured board p = (getBoardData board) !! (indexForPlayerStore p)

testNumCaptured :: Bool
testNumCaptured = 0 == numCaptured initial PlayerA 

{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
allowedMoves :: MancalaBoard -> [Int]
allowedMoves board  
	| p == PlayerA = indices 
	| otherwise = map (+7) indices  
	where 
		p = getCurPlayer board 
		side = playerSide board p 
		indices = findIndices (/=0) $ init side 

testAllowedMoves :: Bool
testAllowedMoves = [0..5] == allowedMoves initial 
		
{- check that a move is valid for the current player -}
isAllowedMove :: MancalaBoard -> Int -> Bool 
isAllowedMove board i = elem i $ allowedMoves board

testIsAllowedMove :: Bool
testIsAllowedMove = not $ isAllowedMove initial 6 

{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}

move :: Int -> MancalaBoard -> MancalaBoard
move i board 
	| not (elem i (allowedMoves board)) = board
	| elem (snd t) [0,7] = MancalaBoardImpl (fst t) p 
	| otherwise = MancalaBoardImpl (fst t) (nextPlayer p)
	where 
		p = getCurPlayer board
		spreadStones (0, layout) m = (layout,m)
		spreadStones (hand, layout) m  
			| isNotOppStore p m = spreadStones (hand-1,incr m layout) $ (m+1) `mod` 14
			| otherwise = spreadStones (hand,layout) $ (m+1) `mod` 14
		t = spreadStones (pickUp board i) (i+1)

testMove :: Bool 
testMove = [0,5,5,5,5,4,0,4,4,4,4,4,4,0] == getBoardData b && PlayerB == getCurPlayer b
	where b = move 0 initial

---- helper functions ----
isNotOppStore p i  
	| i /= indexForPlayerStore (nextPlayer p) = True
	| otherwise = False

pickUp board i = (pitList !! i, rmvStones) where 
	pitList = getBoardData board
	rmvStones = take i pitList ++ [0] ++ drop (i+1) pitList

incr m l = take m l ++ [(l !! m)+1] ++ drop (m+1) l 

{- gameOver checks to see if the game is over -}
gameOver :: MancalaBoard -> Bool
gameOver board = or $ map (all (==0) . init . playerSide board) allPlayers

testGameOver :: Bool 
testGameOver = gameOver (MancalaBoardImpl (replicate 7 0 ++ replicate 7 1) PlayerA)

{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw
 - output based on score -}
winners :: MancalaBoard -> [Player]
winners board
	| head scores > last scores = [PlayerA]
	| head scores < last scores = [PlayerB]
	| otherwise = allPlayers
	where 
		scores = map ( ((getBoardData board) !!) . indexForPlayerStore) allPlayers

testWinners :: Bool 
testWinners = allPlayers == winners initial

testAll = testWinners && testMove && testGameOver && testAllowedMoves && testGameOver && testNumCaptured && 
			testPlayerSide && testIsAllowedMove && testGetBoardData && testGetCurPlayer

---- show ---- 
{- Collaborated with David Lax and James Waugh for the ASCII art -}
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) = -- (show boardData) ++ " " ++ (show player)
            open ++line0 ++line1 ++line2 ++line3 ++line4 ++closer ++tabs ++ show player ++ "'s turn"
      where 
        b = boardData
        open   = "\n" ++ replicate 66 '_' ++ "\n"
        line0 = "/ _______" ++ (concat (replicate 6 (replicate 3 ' ' ++ replicate 5 '_'))) ++ (replicate 10 ' ') ++ "\\ \n"
        line1 = (short line1a 12) ++ short2 line1b (reverse [7..11]) ++ "_]  _______ \\ \n"
        line2 = line2a ++ show (b !! 13) ++ spc ++ "   | \\ \n"
        line3 = line3a ++ show (b !! 6) ++ "  | \\ \n"
        line4 = short line4a 0 ++ (short2 line1b [1..5]) ++ "_]  " ++ bin ++ " \\ \n"
        closer = "/" ++ replicate 66 '_' ++ "\\ \n"
        
        line1a = line2a ++ "   |   [_"
        line1b = "_]   [_"
        line2a = "/ |  "
        spc = "  |" ++ replicate 50 ' ' ++ "|  "
        line3a = "/ " ++ bin ++ (concat (replicate 6 (replicate 3 ' ' ++ replicate 5 '_'))) ++ "  |  "        
        line4a = "/           [_"
        bin = "|_____|"
        tabs = "\t \t \t"

        short str ind     = str ++ show (b !! ind) 
        short2 str (i:is) 
            | (length (i:is) == 1) = str ++ show (b !! i) 
            | otherwise = str ++ show (b !! i) ++ short2 str is

