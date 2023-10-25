{-# LANGUAGE ImplicitPrelude #-}

module Course.ChatBot where

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
commandParser "/stop"  = Just Stop
commandParser "/info"  = Just Info
commandParser str
    | "/input " `isPrefixOf` str = Just (UserInput (drop 7 str))
    | otherwise                 = Nothing

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
    putStrLn "Enter command:"
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