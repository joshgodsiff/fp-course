{-# LANGUAGE ImplicitPrelude #-}

module Course.ChatBot where

import System.IO (hFlush, stdout)
import Data.String (fromString)
import Data.List (isPrefixOf)

data Command 
    = Start
    | UserInput String
    | Stop
    | Info
    deriving (Show, Eq)

commandParser :: String -> Maybe Command
commandParser "/start" = Just Start
commandParser "/info"  = Just Info
commandParser cmd
    | cmd `elem` ["/stop", "/exit", "/quit"] = Just Stop
    | cmd == "/info"                         = Just Info
    | "/input" `isPrefixOf` cmd              = Just (UserInput (drop 7 cmd)) 
    | otherwise                              = Nothing


-- data RulesEngine = RulesEngine
--     { rules :: [Command -> Maybe String]
--     } deriving (Show, Eq)
    
-- data ChatBot = ChatBot 
--     {   commandParser :: CommandsParser, 
--         rulesEngine :: RulesEngine,
--         process :: String -> String 
--     } deriving (Show, Eq)

chatBotMain :: IO ()
chatBotMain = do
    header
    runChatBot

runChatBot :: IO ()
runChatBot = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case commandParser line of
        Just Stop -> putStrLn "Stopping bot..."
        Just cmd -> do
            putStrLn $ "Parsed command: " ++ show cmd
            runChatBot
        Nothing -> do
            putStrLn "Unknown command."
            runChatBot

header :: IO ()
header = do
    putStrLn $ unlines
        [ "╔════════════════════════╗"
        , "║                        ║"
        , "║  Fancy ChatBot v0.1.0  ║"
        , "║                        ║"
        , "╚════════════════════════╝"
        ]