{-# LANGUAGE RecordWildCards #-}

module Graphics.Wayland.XML
    ( Protocol(..)
    , Interface(..)
    , Function(..)
    , Argument(..)
    , ArgType(..)
    , EnumDesc(..)
    , EnumVal(..)
    , parseProtocol
    , parseInterface
    , parseFunc
    , parseArg
    , parseEnum
    , parseEnumVal
    ) where

import Data.Char
import Data.List
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.TypeDefs
--import Text.XML.HXT.Parser.XmlParsec
import Text.XML.HXT.XPath.XPathEval

data Protocol = Protocol
    { protoName :: String
    , protoSummary :: String
    , protoDescription :: String
    , protoCopyright :: String
    , protoInterfaces :: [Interface]
    } deriving (Eq, Ord, Show)

data Interface = Interface
    { ifName :: String
    , ifSummary :: String
    , ifDescription :: String
    , ifVersion :: Int
    , ifRequests :: [Function]
    , ifEvents :: [Function]
    , ifEnums :: [EnumDesc]
    } deriving (Eq, Ord, Show)

data Function = Function
    { funName :: String
    , funSummary :: String
    , funDescription :: String
    , funSince :: Maybe Int
    , funArgs :: [Argument]
    } deriving (Eq, Ord, Show)

data Argument = Argument
    { argName :: String
    , argSummary :: String
    , argDescription :: String
    , argType :: ArgType
    } deriving (Eq, Ord, Show)

data ArgType = ArgInt (Maybe String)
             | ArgUInt (Maybe String)
             | ArgFixed
             | ArgString
             | ArgObject (Maybe String) Bool
             | ArgNewId (Maybe String)
             | ArgArray
             | ArgFd
    deriving (Eq, Ord, Show)

data EnumDesc = EnumDesc
    { enumName :: String
    , enumSummary :: String
    , enumDescription :: String
    , enumBitfield :: Bool
    , enumSince :: Maybe Int
    , enumVals :: [EnumVal]
    } deriving (Eq, Ord, Show)

data EnumVal = EnumVal
    { envalName :: String
    , envalSummary :: String
    , envalDescription :: String
    , envalValue :: String
    } deriving (Eq, Ord, Show)

parseProtocol :: XmlTree -> Protocol
parseProtocol x = Protocol{..}
    where
    protoName = getAttrVal $ head $ getXPath "/protocol/@name" x
    (protoSummary, protoDescription) = getDoc x
    protoCopyright = headDef "" $ map (strip . getTagText) $ getXPath "/protocol/copyright" x
    protoInterfaces = map parseInterface $ getXPath "/protocol/interface" x

parseInterface :: XmlTree -> Interface
parseInterface x = Interface{..}
    where
    ifName = getAttrVal $ head $ getXPath "/interface/@name" x
    (ifSummary, ifDescription) = getDoc x
    ifVersion = head $ map (read . getAttrVal) $ getXPath "/interface/@version" x
    ifRequests = map (parseFunc ifName) $ getXPath "/interface/request" x
    ifEvents = map (parseFunc ifName) $ getXPath "/interface/event" x
    ifEnums = map parseEnum $ getXPath "/interface/enum" x

parseFunc :: String -> XmlTree -> Function
parseFunc iName x = Function{..}
    where
    funName = getAttrVal $ head $ getXPath "/*/@name" x
    (funSummary, funDescription) = getDoc x
    funSince = headMay $ map (read . getAttrVal) $ getXPath "/*/@since" x
    funArgs = map (parseArg iName) $ getXPath "/*/arg" x

parseArg :: String -> XmlTree -> Argument
parseArg iName x = Argument{..}
    where
    argName = getAttrVal $ head $ getXPath "/arg/@name" x
    (argSummary, argDescription) = getDoc x
    argType = case getAttrVal $ head $ getXPath "/arg/@type" x of
      t | t == "int" -> ArgInt enumType
        | t == "uint" -> ArgUInt enumType
        | t == "fixed" -> ArgFixed
        | t == "string" -> ArgString
        | t == "object" -> ArgObject objType allowNull
        | t == "new_id" -> ArgNewId objType
        | t == "array" -> ArgArray
        | t == "fd" -> ArgFd
        | otherwise -> error $ "parseArg: unknown type: " ++ t
    objType = headMay $ map getAttrVal $ getXPath "/arg/@interface" x
    allowNull = headDef False $ map (strBool . getAttrVal) $ getXPath "/arg/@allow-null" x
    enumType = fmap (insType . getAttrVal) $ headMay $ getXPath "/arg/@enum" x
    insType t = if '.' `elem` t
                   then t
                   else iName ++ "." ++ t

parseEnum :: XmlTree -> EnumDesc
parseEnum x = EnumDesc{..}
    where
    enumName = getAttrVal $ head $ getXPath "/enum/@name" x
    (enumSummary, enumDescription) = getDoc x
    enumBitfield = headDef False $ map (strBool . getAttrVal) $ getXPath "/enum/@bitfield" x
    enumSince = headMay $ map (read . getAttrVal) $ getXPath "/enum/@since" x
    enumVals = map parseEnumVal $ getXPath "/enum/entry" x

parseEnumVal :: XmlTree -> EnumVal
parseEnumVal x = EnumVal{..}
    where
    envalName = getAttrVal $ head $ getXPath "/entry/@name" x
    (envalSummary, envalDescription) = getDoc x
    envalValue = getAttrVal $ head $ getXPath "/entry/@value" x

getDoc :: XmlTree -> (String, String)
getDoc x = if null $ getXPath "/*/description" x
              then (maybe "" getAttrVal $ headMay $ getXPath "/*/@summary" x,"")
              else (getAttrVal $ head $ getXPath "/*/description/@summary" x
                   ,strip $ getTagText $ head $ getXPath "/*/description" x)

getAttrVal :: XmlTree -> String
getAttrVal (NTree (XAttr _) [NTree (XText name) []]) = name
getAttrVal x = error $ "getAttrVal: invalid attribute: " ++ show x

getTagText :: XmlTree -> String
getTagText (NTree (XTag _ _) [NTree (XText name) []]) = name
getTagText _ = ""

headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay [] = Nothing

headDef :: a -> [a] -> a
headDef _ (x:_) = x
headDef x [] = x

strip :: String -> String
strip = unlines . dropWhileEnd null . dropWhile null . map (dropWhile isSpace) . lines

strBool :: String -> Bool
strBool s
    | s == "true" = True
    | s == "false" = False
    | otherwise = error $ "not a bool: " ++ s
