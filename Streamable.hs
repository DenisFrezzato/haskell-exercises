{-# LANGUAGE FlexibleInstances #-}

import Data.Either
import Data.Monoid
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.IO
import System.Environment
import Control.Monad
import Control.Exception

class (Monoid a, Show a) => Streamable a where
    readChunk :: Handle -> Int -> IO a
    null :: a -> Bool

instance Streamable String where
    readChunk h size = T.unpack . TE.decodeUtf8 <$> B.hGetSome h size
    null = L.null

instance Streamable T.Text where
    readChunk h size = TE.decodeUtf8 <$> B.hGetSome h size  
    null = T.null

instance Streamable TL.Text where
    readChunk h size = TLE.decodeUtf8 <$> BL.hGet h size
    null = TL.null

instance Streamable B.ByteString where
    readChunk = B.hGetSome
    null = B.null

instance Streamable BL.ByteString where
    readChunk = BL.hGet
    null = BL.null

data StreamException = EOFException | OtherException SomeException deriving (Show)

instance Exception StreamException

{- streamFile path bytes
Given a file located at `path` and a chunk size `bytes`, `makeFileStream` will:
return an IO action, which, when executed, will return `Either StreamException a`
If successful, it will have read `bytes` from the given file and returned some
`Streamable a` data, otherwise it will return StreamException.
Make sure that your `StreamException` can encapsulate the case when the file has
been fully read (reached EOF), which will signal to the caller that they should stop.
`makeFileStream` should never throw an exception: all exceptions should be
abstracted into the `StreamException` type.
-}
makeFileStream :: Streamable a => FilePath -> Int -> IO (IO (Either StreamException a))
makeFileStream filepath chunkSize = do
   handle <- openFile filepath ReadMode
   hSetBinaryMode handle True
   return $ readNext handle
       where
           readNext :: Streamable a => Handle -> IO (Either StreamException a)
           readNext h = do
               eRes <- try $ readChunk h chunkSize
               case eRes of
                 Left e -> safeClose h >> return (Left $ OtherException e)
                 Right a 
                     | Main.null a -> safeClose h >> return (Left EOFException)
                     | otherwise -> return (Right a)

           safeClose :: Handle -> IO (Either StreamException ())
           safeClose h = do
               eRes <- try $ hClose h
               case eRes of
                 Left e -> return $ Left $ OtherException e
                 Right _ -> return $ Right ()

main = void $ do
    args <- getArgs
    streamer <- makeFileStream (head args) 2048
    run streamer where
        run :: IO (Either StreamException B.ByteString) -> IO ()
        run streamer = do
            eChunk <- streamer
            case eChunk of
              Left e -> print e
              Right a -> print a >> run streamer
