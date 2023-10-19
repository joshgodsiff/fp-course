{-# LANGUAGE ImplicitPrelude #-}

module Course.ChatBot where

import Data.String (fromString)

helloWorld :: IO ()
helloWorld = print "Hello World!"

data Command 
    = Start
    | UserInput String
    | Stop
    | Info
    deriving (Show, Eq)

commandParser :: String -> Maybe Command
commandParser "/start" = Just (Command Start)
commandParser "/stop"  = Just (Command Stop)
commandParser "/info"  = Just (Command Info)
commandParser str
    | "/input " `isPrefixOf` str = Just (Command (UserInput (drop 7 str)))
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
    putStrLn "Enter command:"
    line <- getLine
    case parse commandParser line of
        Just (Command Stop) -> putStrLn "Stopping bot..."
        Just cmd -> do
            putStrLn $ "Parsed command: " ++ show cmd
            chatBotMain commandParser
        Nothing -> do
            putStrLn "Unknown command."
            chatBotMain commandParser