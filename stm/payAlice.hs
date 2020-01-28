-- Second exercise from https://tech.fpcomplete.com/haskell/library/stm

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)

main :: IO ()
main = do
  aliceVar <- newTVarIO 0
  bobVar <- newTVarIO 0

  _ <- forkIO $ payAlice aliceVar

  atomically $ do
    currentAlice <- readTVar aliceVar
    check $ currentAlice >= 20
    writeTVar aliceVar (currentAlice - 20)
    currentBob <- readTVar bobVar
    writeTVar bobVar (currentBob + 20)

  finalAlice <- atomically $ readTVar aliceVar
  finalBob <- atomically $ readTVar bobVar

  putStrLn $ "Final Alice: " ++ show finalAlice
  putStrLn $ "Final Bob: " ++ show finalBob

payAlice :: TVar Int -> IO ()
payAlice aliceVar = forever $ do
  threadDelay 1000
  atomically $ do
    current <- readTVar aliceVar
    writeTVar aliceVar (current + 5)
  putStrLn "Paid Alice"

