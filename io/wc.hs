{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

main = B.readFile "source.txt" >>=
    putStrLn . ("Number of lines: " <>) . show . length . T.lines . TE.decodeUtf8
