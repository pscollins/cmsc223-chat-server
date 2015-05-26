{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck

import System.IO.Temp
import Data.IORef
import System.Directory

import Chat

tempHandleDir :: String
tempHandleDir = "test/generated"

tempHandlePattern :: String
tempHandlePattern = "clientFile.tmp"

withTempClient :: Int -> (Client -> IO a) -> IO a
withTempClient cIdx f = withTempFile tempHandleDir tempHandlePattern axn
  where axn _ handle = f $ Client cIdx handle

withTempClients :: [Int] -> ([Client] -> IO a) -> IO a
withTempClients idxs f = do
  (paths, handles) <-
    mapM (\_ -> openTempFile tempHandleDir tempHandlePattern) idxs >>= return . unzip
  res <- f $ zipWith Client idxs handles
  mapM_ removeFile paths
  return res

testPrefixMessage :: Spec
testPrefixMessage =
  describe "prefix message" $ do
    it "prefixes 1" $ withTempClient 1 $ \c -> do
      prefixMessage c "hello" `shouldBe` "1: hello"
    it "prefixes 2" $ withTempClient 2 $ \c -> do
      prefixMessage c "hello" `shouldBe` "2: hello"

testOtherClients :: Spec
testOtherClients =
  describe "other clients" $ do
    it "ignores itself" $ withTempClients [1..3] $ \cs@[c1, c2, c3] -> do
      cRef <- newIORef cs
      otherClients c1 cRef `shouldReturn` [c2, c3]
      otherClients c2 cRef `shouldReturn` [c1, c3]
      otherClients c3 cRef `shouldReturn` [c1, c2]

testAddClient :: Spec
testAddClient =
  describe "add client" $ do
    it "adds a client" $ withTempClient 1 $ \c -> do
      cRef <- newIORef ([] :: [Client])
      readIORef cRef `shouldReturn` []
      addClient c cRef
      readIORef cRef `shouldReturn` [c]


main :: IO ()
main = hspec $ describe "Testing Lab 2" $ do
  testPrefixMessage
  testOtherClients
  testAddClient
