{-# LANGUAGE ScopedTypeVariables #-}
-- | Author: Patrick Collins
-- Purpose: Simple TCP chat server.
module Chat where

import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Data.IORef
import Network
import System.Environment (getEnv)
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
-- prefixMessage (Client idxNum _) = (((show idxNum) ++ ": ") ++)

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
-- message and echo it to the other clients.)
-- We split it out for the sake of unit testing
talkAction' :: (String -> Client -> IO ()) -> (String -> String)
               -> (IORef [Client] -> IO [Client])
               -> IORef [Client] -> Client -> IO ()
talkAction' doTell prefixMe everyoneElse currentClients me = do
  toSay <- askClient me >>= return . prefixMe
  putStrLn $ "About to say: " ++ toSay
  everyoneElse currentClients >>= mapM_ (doTell toSay)

-- | We need to use a version of tellClient that rewinds the file
-- handle when we unit test, but our "real code" will only ever call
-- this.
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
chatLoop socket = newIORef ([] :: [Client]) >>= (\cRef -> mapM_ (getConnection cRef) [1..])
  where
    getConnection cRef clientIdx = do
      putStrLn $ "About to connect to client" ++ (show clientIdx)
      (handle, hostName, _) <- accept socket
      putStrLn $ "Got a client from host " ++ hostName
      hSetBuffering handle NoBuffering
      let newClient = Client clientIdx handle
      addClient newClient cRef
      sayHi newClient cRef
      newThIdx <-
        forkFinally (talk newClient cRef) (\_ -> finalize newClient cRef)
      putStrLn $ "Forked thread " ++ show newThIdx

-- | Run the chat server on a particular port. Lets us split out the
-- port-finding function for testing.
chatOnPort :: Int -> IO ()
chatOnPort port = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  chatLoop sock

-- | Grab the preferred port to the environment.
getPort :: IO Int
getPort = getEnv "CHAT_SERVER_PORT" >>= return . read

-- | Entry point for the chat server.
chat :: IO ()
chat = getPort >>= chatOnPort
