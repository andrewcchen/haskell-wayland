module Main where

import Data.Char
import Data.List
import Data.List.Split hiding (startsWith)
import Data.String
import System.Environment


join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

strip :: (a -> Bool) -> [a] -> [a]
strip p = dropWhileEnd p . dropWhile p

isIdent, isNotIdent :: Char -> Bool
isIdent c = isAlphaNum c || '_' == c
isNotIdent = not . isIdent

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

processType :: String -> String
processType s
    | s == "void" = "()"
    | s `startsWith` "struct " = if '*' `elem` s
        then capitalize $ snakeToCamel
           $ takeWhile isIdent $ dropWhile isNotIdent $ dropWhile isIdent s
        else error $ "structs are not supported: " ++ s
    | '*' `elem` s = "Ptr ()"
    | s `startsWith` "int " || s == "int" = "Int32"
    | s `startsWith` "int32_t " = "Int32"
    | s `startsWith` "uint32_t" = "Word32"
    | otherwise = error $ "type not supported: " ++ s

type FuncSig = (String, String, [String])

processFuncPtr :: String -> FuncSig
processFuncPtr s = (snakeToCamel name, processType $ strip isSpace ret, params)
    where
    (ret, s1) = span (/= '(') $ strip isSpace s
    (name, s2) = span isIdent $ dropWhile isNotIdent s1
    params = map (processType . strip isNotIdent) $ splitOn "," $ strip isNotIdent s2

funcSigToType :: String -> FuncSig -> String
funcSigToType m (_,ret,parms) = join " -> " parms++" -> "++wrapRet ret
    where wrapRet r = if ' ' `elem` r then m++" ("++r++")" else m++" "++r

emitData :: String -> [FuncSig] -> String
emitData name funcs = join "\n" $ decl : map ("    "++) body
    where
    decl = "data "++name++"Interface m = "++name++"Interface"
    body = (map (\(i,x) -> (if i==0 then "{ " else ", ") ++ f x) $ zip [0..] funcs) ++ ["}"]
    f x@(name,_,_) = name++" :: "++funcSigToType "m" x

emitImport :: String -> [FuncSig] -> String
emitImport name funcs = join "\n" $ map ("foreign import ccall \"wrapper\" "++) $ map f $ zip [0..] funcs
    where
    f (i, x) = fname i++" :: ("++funcSigToType "IO" x++") -> IO (FunPtr ("++funcSigToType "IO" x++"))"
    fname i = "mk"++name++"Func"++show i

emitInstance :: String -> [FuncSig] -> String
emitInstance iname funcs = join "\n" $ decl : fbod : map ("        "++) body
    where
    decl = "instance MonadWayland s m => InterfaceImplementation ("++iname++"Interface m) m where"
    fbod = "    wrapImplementation i = wrapCallback <$> getStateRef >>= \\wcb -> liftIO $ sequence"
    body = (map (\(i,x) -> (if i==0 then "[ " else ", ") ++ f i x) $ zip [0..] funcs) ++ ["]"]
    f i (name,_,parms) = "fmap castFunPtr $ mk"++iname++"Func"++show i++" $ wcb "++dots parms++name++" i"
    dots x = if length x == 0 then "" else "."++take ((length x)-1) (repeat ':')++" "

emitDataImportInstance name funcs = join "\n\n" $ map (\f -> f name funcs) [emitData,emitImport,emitInstance]

main = do
    args <- getArgs
    let name = if length args > 0
                  then args !! 0
                  else "NameHere"
    ls <- lines <$> getContents
    let fs = map processFuncPtr ls
    putStrLn $ emitDataImportInstance name fs
