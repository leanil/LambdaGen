{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, OverloadedStrings, StandaloneDeriving, TypeApplications, TypeOperators #-}

module Server where

import Compile
import Expr
import Print
import Recursion
import Typecheck
import Data.Proxy (Proxy(Proxy))
import qualified Network.WebSockets as WS
import Data.Aeson (FromJSON, Value(..), (.:), eitherDecode, parseJSON, withArray, withObject)
import Data.Aeson.Types (Parser)
import Data.Functor.Foldable (ana)
import Data.HashMap.Strict (HashMap, (!), elems, empty, insert, keys)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vinyl (Rec(RNil))
import System.FilePath ((</>))

main :: IO ()
main = do
    WS.runServer "localhost" 2103 $ compiler

compiler :: WS.ServerApp
compiler pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    let d = eitherDecode msg :: Either String Diagram
    case d of
        Left err -> putStrLn err
        Right ps -> do
            result <- compile (".." </> "test") "eval" (buildExpr ps)
            case result of
                Just expr -> putStr $ printExpr (Proxy :: Proxy (R '[TypecheckT])) expr
                Nothing -> return ()

buildExpr :: Diagram -> Expr0
buildExpr (Diagram links nodes) = ana helper root where
    helper :: CoAlgebra Expr0 (ExprF (), Text)
    helper (Scalar x, _) = RNil ::< Scalar x
    helper (ScalarOp x _ _, i) = RNil ::< ScalarOp x (getChildWithId i "left") (getChildWithId i "right")
    root = let i = head $ filter (\x -> notElem x $ elems links) $ keys nodes in (getExpr $ nodes ! i, i)
    getChildWithId :: Text -> Text -> (ExprF (), Text)
    getChildWithId node child = 
        let i = links ! (ports (nodes ! node) ! child) in (getExpr $ nodes ! i, i)

data Diagram = Diagram { getLinks :: HashMap Text Text, getNodes :: HashMap Text Node } deriving Show

data Node = Node { ports :: HashMap Text Text, getExpr :: ExprF () } deriving Show

instance FromJSON (ExprF ()) where
    parseJSON = withObject "ExprF" $ \v -> case v ! "tag" of
        (String "Scalar") -> Scalar <$> v .: "getSclVal"
        (String "ScalarOp") -> (\x -> ScalarOp x () ()) <$> v .: "getOp"

instance FromJSON Node where
    parseJSON = withObject "Node" $ \o -> Server.Node <$> parsePorts (o ! "ports") <*> parseJSON (o ! "expr")

parseLinks :: Value -> Parser (HashMap Text Text)
parseLinks = withArray "links" $ V.foldM 
    (\a b -> withObject "link"
        (\o -> insert <$> o .: "targetPort" <*> o .: "source" <*> pure a)
        b) empty

parseNodes :: Value -> Parser (HashMap Text Node)
parseNodes = withArray "nodes" $ V.foldM 
    (\a b -> withObject "node"
        (\o -> insert <$> o .: "id" <*> parseJSON b <*> pure a)
        b) empty

parsePorts :: Value -> Parser (HashMap Text Text)
parsePorts = withArray "ports" $ V.foldM 
    (\a b -> withObject "port"
        (\o -> insert <$> o .: "name" <*> o .: "id" <*> pure a)
        b) empty

instance FromJSON Diagram where
    parseJSON = withObject "Diagram" $ \o -> Diagram <$> parseLinks (o ! "links") <*> parseNodes (o ! "nodes")
