import Network.HTTP
import Text.HTML.TagSoup
import Data.Char
import Control.Concurrent

-- TODO: * Use cache
--       * Make function return a value instead of printing out the value
--         (issue is not so much to have this function return a value, rather
--          integrating it with words and unwords)
wordlookup :: String -> IO ()
wordlookup word = do
    let url = "http://woordenlijst.org/zoek/?q=" ++ word ++ "&w=w"
    putStrLn $ "lookup word '"++ word ++"' => query: " ++ url
    -- Some magic code which I don't yet fully understand the internal but it gets the full result
    -- First time I come across =<<
    src <- getResponseBody =<< simpleHTTP (getRequest url)
    -- Parse the result
    let definition = fromWordDef $ parseTags src
    putStrLn $ "Definition for '"++ word ++"': " ++ definition ++ ".\n"
    -- To be improved: Wait <x> seconds after doing query 
    -- (annoying because we will wait <x>s too much for the last on but it will do for now)
    threadDelay $ 1 * 1000 * 1000
    -- dropWhile: the text we are interest in is burried inside 
    --            <span style='background-color:#785f38;color:#fff;'><strong>architect</strong> 
    --            [ar·chi·tect], de[m.], architecten [ar·chi·tec·ten]</span><br>
    -- take 5   : [1] the span tag, 
    --            [2] the strong tag, 
    --            [3] the strong text, 
    --            [4] the strong end tag, 
    --            [5] the actual text
    where fromWordDef = innerText . take 5 . dropWhile (~/= "<span style='background-color:#785f38;color:#fff;'>")

main = do  
    line <- getLine
    mapM wordlookup $ words line

