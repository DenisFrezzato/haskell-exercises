{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>))
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

findLongest :: [T.Text] -> Int
findLongest = foldl (\longest line -> 
    let lineLength = T.length line
    in if lineLength > longest then lineLength else longest) 0


-- Find the length of the longest line in a file.
main = B.readFile "source.txt" >>= (
    TE.decodeUtf8
    >>> T.lines
    >>> findLongest
    >>> print)
