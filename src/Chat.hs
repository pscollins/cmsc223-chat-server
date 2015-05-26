{-# LANGUAGE ScopedTypeVariables #-}
-- | CS240h Lab 2 Chat Server
module Chat where

import Network
import Control.Concurrent (forkFinally)
import Data.IORef
import System.IO
import System.Environment (getEnv)
import Control.Monad (forever)

-- Note that we *dont* need any locks on our handle since GHC gives
-- them to us already
-- http://hackage.haskell.org/package/base-4.3.1.0/docs/src/GHC-IO-Handle-Types.html
data Client = Client {idx :: Int, hClient :: Handle} deriving (Show, Eq)

-- atomicModifyIORef saves us from a lock here
addClient :: Client -> IORef [Client] -> IO ()
addClient c clientsRef = atomicModifyIORef clientsRef $ \cs -> (c:cs, ())

prefixMessage :: Client -> String -> String
prefixMessage (Client idxNum _) = (((show idxNum) ++ ": ") ++)

otherClients :: Client -> IORef [Client] -> IO [Client]
otherClients (Client idxNum _) clients =
  readIORef clients >>= return . filter ((idxNum /=) . idx)

tellClient :: String -> Client -> IO ()
tellClient = flip $ hPutStrLn . hClient

askClient :: Client -> IO (String)
askClient = hGetLine . hClient

-- We split this out for the sake of unit testing
talkAction :: (String -> String) -> (IORef [Client] -> IO [Client])
              -> IORef [Client] -> Client -> IO ()
talkAction prefixMe everyoneElse currentClients me = do
  toSay <- askClient me >>= return . prefixMe
  putStrLn $ "About to say: " ++ toSay
  everyoneElse currentClients >>= mapM_ (tellClient toSay)

talk :: Client -> IORef [Client] -> IO ()
talk me currentClients = doTalk
  where prefixMe = prefixMessage me
        everyoneElse = otherClients me
        doTalk = forever $ talkAction prefixMe everyoneElse currentClients me

chatLoop :: Socket -> IO ()
chatLoop socket = loop [1..]
  where
    currentClients' :: IO (IORef [Client])
    currentClients' = newIORef ([] :: [Client])
    loop [] = return ()         -- Suppress warning
    loop (clientIdx:idxs) = do
      putStrLn $ "About to connect to client" ++ (show clientIdx)
      (handle, hostName, _) <- accept socket
      putStrLn $ "Got a client from host " ++ hostName
      currentClients <- currentClients'
      hSetBuffering handle NoBuffering
      let newClient = Client clientIdx handle
      addClient newClient currentClients
      newThIdx <-
        forkFinally (talk newClient currentClients) (\_ -> hClose handle)
      putStrLn $ "Forked thread " ++ show newThIdx
      loop idxs

chatOnPort :: Int -> IO ()
chatOnPort port = withSocketsDo $ do
  sock <- listenOn $ PortNumber $ fromIntegral port
  chatLoop sock

getPort :: IO Int
getPort = getEnv "CHAT_SERVER_PORT" >>= return . read

chat :: IO ()
chat = getPort >>= chatOnPort
