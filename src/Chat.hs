{-# LANGUAGE ScopedTypeVariables #-}
-- | Author: Patrick Collins
-- Purpose: Simple TCP chat server.
-- Note that we export everything from this module on purpose
-- because the only reason we factored it out in to a separate file
-- was so that we could test it either. Our only user is the Main.hs
-- file in the directory above us, we are not intended to be used as a
-- library, so there is no reason to keep our interface narrow. We
-- only have one friend, and we tell him everything.
-- We break our code up in to many small, reusable functions for the
-- sake of testing, because **untestable code is bad code.**
module Chat where

import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Data.IORef
import Network
import System.Environment (lookupEnv)
import System.IO

-- | Data type for clients connected to the server.
-- Note that we *dont* need any locks on our handle since GHC gives
-- them to us already. See the GHC.IO.Handle.Types source; Handle is
-- implemented with an MVar.
data Client = Client {idx :: Int, hClient :: Handle} deriving (Show, Eq)

-- | Add a client to the list of connected clients. Using
-- atomicModifyIORef saves us from a lock here
addClient :: Client -> IORef [Client] -> IO ()
addClient c clientsRef = atomicModifyIORef clientsRef $ \cs -> (c:cs, ())

-- | Remove a client from the list of connected clients.
removeClient :: Client -> IORef [Client] -> IO ()
removeClient c clientsRef =
  atomicModifyIORef clientsRef $ \cs -> (notMe c cs, ())

-- | Generate the "<C.username>: " message prefix.
prefixMessage :: Client -> String -> String
prefixMessage = (++) . (sayForClient ": ")

-- | Grab every client from a list of clients except for the current
-- one.
notMe :: Client -> [Client] -> [Client]
notMe (Client idxNum _) = filter ((idxNum /=) . idx)

-- | Wrapper around notMe to interact with the IORef.
otherClients :: Client -> IORef [Client] -> IO [Client]
otherClients c clients =
  readIORef clients >>= return . (notMe c)

-- | Send a message to a client.
tellClient :: String -> Client -> IO ()
tellClient = flip $ hPutStrLn . hClient

-- | Recieve a message sent to a client.
askClient :: Client -> IO (String)
askClient = hGetLine . hClient

-- | Perform one step of the client's event loop (i.e. recieve a
-- message and echo it to the other clients.) We split it out for the
-- sake of unit testing.
talkAction' :: (String -> Client -> IO ()) -> (String -> String)
               -> (IORef [Client] -> IO [Client])
               -> IORef [Client] -> Client -> IO ()
talkAction' doTell prefixMe everyoneElse currentClients me = do
  toSay <- askClient me >>= return . prefixMe
  everyoneElse currentClients >>= mapM_ (doTell toSay)

-- | We need to be able to make a version of tellClient that rewinds
-- the file handle when we unit test, but our "real code" will only
-- ever call this.
talkAction :: (String -> String)
              -> (IORef [Client] -> IO [Client])
              -> IORef [Client] -> Client -> IO ()
talkAction = talkAction' tellClient


-- | Client event loop.
talk :: Client -> IORef [Client] -> IO ()
talk me currentClients = doTalk
  where prefixMe = prefixMessage me
        everyoneElse = otherClients me
        doTalk = forever $ talkAction prefixMe everyoneElse currentClients me

-- | Send a message to every other client
tellAll :: String -> IORef [Client] -> IO ()
tellAll msg cRef = readIORef cRef >>= mapM_ (tellClient msg)

-- | Prefix a string with a client's index.
sayForClient :: String -> Client -> String
sayForClient msg (Client cIdx _) = show cIdx ++ msg

-- | Send a message with the given mapping between clients and strings
-- (i.e. either "<C.username> has joined" or "<C.username> has left)
-- to all clients.
sayWith :: (Client -> String) -> Client -> IORef [Client] -> IO ()
sayWith = (tellAll .)

-- | Send the new connection notification to all clients.
sayHi :: Client -> IORef [Client] -> IO ()
sayHi = sayWith $ sayForClient " has joined"

-- | Send the connection closed notifcation to all clients.
sayBye :: Client -> IORef [Client] -> IO ()
sayBye = sayWith $ sayForClient " has left"

-- | Send the appropriate message for a client that has left and free
-- up resources.
finalize :: Client -> IORef [Client] -> IO ()
finalize c@(Client _ hC) cRef = hClose hC >> removeClient c cRef >> sayBye c cRef

-- | Main loop for the chat server.
chatLoop :: Socket -> IO ()
chatLoop socket = mkCRef >>= (\cRef -> mapM_ (getConnection cRef) [1..])
  where
    mkCRef = newIORef ([] :: [Client])
    getConnection cRef clientIdx = do
      (handle, _, _) <- accept socket
      hSetBuffering handle NoBuffering
      let newClient = Client clientIdx handle
      addClient newClient cRef
      sayHi newClient cRef
      forkFinally (talk newClient cRef) (\_ -> finalize newClient cRef)


-- | Run the chat server on a particular port. Lets us split out the
-- port-finding function for testing.
chatOnPort :: Int -> IO ()
chatOnPort port = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  chatLoop sock

-- | Grab the preferred port to the environment.
getPort :: IO (Maybe Int)
getPort = lookupEnv "CHAT_SERVER_PORT" >>= return . fmap read

-- | Entry point for the chat server.
chat :: IO ()
chat = do
  runPort <- getPort
  case runPort of
   Just p -> chatOnPort p
   Nothing -> putStrLn "Environment variable CHAT_SERVER_PORT not set."
