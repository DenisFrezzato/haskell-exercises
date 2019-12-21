{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B

sanitise :: B.ByteString -> B.ByteString
sanitise = B.map (\w -> if w == space then underscore else w) where
    space = 32
    underscore = 95 

sanitiseFile :: IO ()
sanitiseFile = do
    fileContent <- B.readFile "io.txt" 
    B.writeFile "io-sanitised.txt" $ sanitise fileContent

main = sanitiseFile

