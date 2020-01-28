-- First exercise from https://tech.fpcomplete.com/haskell/library/stm

import Control.Concurrent.STM
import Control.Monad (replicateM_)

makeCounter :: IO (IO Int)
makeCounter = do
    var <- newTVarIO 1
    return $ atomically $ do
        count <- readTVar var
        writeTVar var $ count + 1
        return count

main :: IO ()
main = do
    counter <- makeCounter
    replicateM_ 10 $ counter >>= print
