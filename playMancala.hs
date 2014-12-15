module Main where

import MancalaBoard 
import Control.Monad.State
import Data.Char
import Data.List (intercalate)
import System.IO
import Text.Regex.Posix

{- type declarations -}

data InternalState = InternalState { stack :: [MancalaBoard] }

type BoardState = State InternalState

{- private functions -}

pop :: BoardState MancalaBoard
pop = state $ \st -> case stack st of
	[] -> (initial,st)
	x:xs -> (x,st { stack = xs })

push :: MancalaBoard -> BoardState ()
push d = modify $ \st -> st { stack = d : stack st }

edit :: Int -> BoardState ()
edit n = modify $ \st -> st { stack = (move n (head (stack st))) : stack st}

{- IO -}

type IOBoardState = StateT InternalState IO

while :: Monad m => m Bool -> m ()
while action = loop where
	loop = do
		continue <- action
		when continue loop

printState :: IOBoardState ()
printState = do
	state <- get
	liftIO $ do
		putStrLn $ "s = " ++ (intercalate " " $ map show $ stack state)

printBoard :: IOBoardState ()
printBoard = do
	state <- get
	liftIO $ do
		putStrLn $ show $ head $ stack state 

isEndGame :: IOBoardState Bool
isEndGame = do 
	state <- get
	return $ gameOver $ head $ stack state

getWinners :: IOBoardState [Player]
getWinners = do
	state <- get
	return $ winners $ head $ stack state

runCommands :: [String] -> IOBoardState Bool
runCommands [] = return True
runCommands (c:cs)
	| c =~ "^[0-9]+$" = run $ edit (read c)
	| c == "p" = printState >> runCommands cs
	| c == "q" = return False
	| otherwise = do
		liftIO $ do
			putStrLn $ "unrecognized command: " ++ c
			putStrLn $ "commands not executed: " ++ show cs
		return True
	where
		run cmd = shift cmd >> fork
		shift = state . runState
		fork = do
			bool <- isEndGame
			if bool then do 
				printBoard
				whoWins <- getWinners
				liftIO $ do
					putStrLn $ "WINNER(S): " ++ (show whoWins)
					return False
			else 
				runCommands cs

repl :: IOBoardState ()
repl = while $ do
	printBoard
	liftIO $ putStr ": " >> hFlush stdout
	commands <- liftIO (fmap words getLine)
	runCommands commands

startState :: InternalState
startState = InternalState { stack = [initial] }

withInputBuffering :: MonadIO m => BufferMode -> m a -> m a
withInputBuffering mode action = do
    savedMode <- liftIO $ hGetBuffering stdin
    liftIO $ hSetBuffering stdin mode
    result <- action
    liftIO $ hSetBuffering stdin savedMode
    return result

main :: IO ()
main = 	void $ withInputBuffering LineBuffering $ runStateT repl startState

