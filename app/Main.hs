import Control.Concurrent
import Control.Monad
import System.Random
import Data.Time.Clock
import Data.Time.Format

-- | Represention of a user using username
data User = User { 
    userName :: UserName -- ^  user's name.
} deriving (Eq, Show)

-- | Defining the type username.
type UserName = String

-- | Defining the type message.
type Message = String

-- | flexible variable containing a list of tuples containing the message counts and names of the users.

type MessageCounts = MVar [(UserName, Int)]

-- | flexible variable containing a list of tuples containing the total number of messages sent.
type TotalMessages = MVar Int

-- | Simulation of sending a message.Prints the message and modifies the shared state.
--    Updates the recipient's message count.
sendMessage :: MessageCounts -- ^ Mutable variable holding message counts.
            -> User          -- ^ Sender of the message.
            -> User          -- ^ Receiver of the message.
            -> Message       -- ^ The message to be sent.
            -> IO ()
sendMessage msgCountVar sender receiver msg = do
    msgCounts <- takeMVar msgCountVar
    let updatedCounts = updateMessageCount (userName receiver) msgCounts
    putMVar msgCountVar updatedCounts
    
    timestamp <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
    putStrLn $ formattedTime ++ " - " ++ userName sender ++ " sends to " ++ userName receiver ++ ": " ++ msg

-- |  updates the specific recipient's message count. 
updateMessageCount :: UserName               -- ^ The user name of reciever.
                   -> [(UserName, Int)]      -- ^  list of current message counts.
                   -> [(UserName, Int)]      -- ^  list of updated message counts.
updateMessageCount receiver msgCounts = case lookup receiver msgCounts of
    Just count -> map (\(user, cnt) -> if user == receiver then (user, cnt + 1) else (user, cnt)) msgCounts
    Nothing -> (receiver, 1) : msgCounts

-- | describes how a user thread behaves in the messaging simulation.
userThread :: [User]         -- ^ List of all users.
           -> User           -- ^ The current user running this thread.
           -> MessageCounts  -- ^ Mutable variable holding message counts.
           -> TotalMessages  -- ^ Mutable variable holding the total message count.
           -> IO ()
userThread users currentUser msgCountVar totalMsgVar = forever $ do
    delay <- randomRIO (1, 5) -- Random delay between 1 to 5 seconds
    threadDelay (delay * 1000000)

    -- a random user is choosen to send a message
    receiver <- randomChoice $ filter (/= currentUser) users
    let message = "Hello from " ++ userName currentUser
    sendMessage msgCountVar currentUser receiver message

    -- total message count is incremented
    totalMsgs <- takeMVar totalMsgVar
    if totalMsgs >= 100 then do
        putMVar totalMsgVar totalMsgs
    else do
        putMVar totalMsgVar (totalMsgs + 1)

-- | a random element from a list is chosen
randomChoice :: [a]  -- ^ The list to choose from.
             -> IO a -- ^ The randomly chosen element.
randomChoice list = do
    idx <- randomRIO (0, length list - 1)
    return $ list !! idx

-- | The main program. it intialises the users, shared state, and starts user threads.
main :: IO ()
main = do
    -- Assign usernames to users using the following format: "User: <number>"
    let users = map (\n -> User $ "User: " ++ show n) [1..10]
    msgCountVar <- newMVar []
    totalMsgVar <- newMVar 0

    -- Spawn user threads
    mapM_ (\user -> forkIO $ userThread users user msgCountVar totalMsgVar) users

    -- Wait until 100 messages are sent
    let waitUntilDone = do
            totalMsgs <- readMVar totalMsgVar
            when (totalMsgs < 100) $ do
                threadDelay 1000000  -- Wait for 1 second
                waitUntilDone
    waitUntilDone

    -- Output of final message counts
    finalCounts <- readMVar msgCountVar
    putStrLn "\n\nFinal message counts:"
    mapM_ (putStrLn . show) finalCounts
