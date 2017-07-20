module Main where

import Data.Char
import Data.List
import Data.List.Split hiding (startsWith)
import Data.String
import System.Environment

import Data.Tree.Class
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.XPath.XPathEval


join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

strip :: (a -> Bool) -> [a] -> [a]
strip p = dropWhileEnd p . dropWhile p

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (a:as) (b:bs)
    | a == b = startsWith as bs
    | otherwise = False

mapAllButFirst :: (a -> a) -> [a] -> [a]
mapAllButFirst _ [] = []
mapAllButFirst f (x:xs) = x : map f xs

snakeToCamel :: String -> String
snakeToCamel = foldr (++) "" . mapAllButFirst capitalize . splitOn "_"

snakeToCamel' :: String -> String
snakeToCamel' = foldr (++) "" . map capitalize . splitOn "_"



protocolName = snakeToCamel' . get . head . getChildren . head . getXPath "/protocol/@name"
    where get (NTree (XText x) _) = x

type RequestFunc = 

main = do
    args <- getArgs
    let filename = head args
    file' <- readFile filename
    let file = if file' `startsWith` "<?"
                  then unlines $ tail $ lines file
                  else file'
        xml = head $ xread file
    putStrLn $ show $ head $ getXPath "/protocol/@name" xml
    putStrLn $ protocolName xml
