import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import System.Timeout

threadDelayMS :: Int -> IO ()
threadDelayMS ms = threadDelay $ ms * 1000

handleThreadEnd :: Either SomeException a -> IO ()
handleThreadEnd (Left e) = print e
handleThreadEnd _ = pure ()

main = do
    a1 <- async $ do
        m <- timeout 2000000 $ threadDelayMS 2500
        case m of
          Just _ -> putStrLn "Forked IO #1 has done."
          Nothing -> putStrLn "Forked IO #1 has timed out."
    a2 <- async $ threadDelayMS 1500 >> putStrLn "Forked IO #2 has done."
    a3 <- async $ threadDelayMS 1000 >> putStrLn "Forked IO #3 has done."
    putStrLn "Waiting..."
    sequence_ [wait a1, wait a2, wait a3]
