{-# LANGUAGE OverloadedStrings #-}

module ParseTTreeHeader where

import Data.Text (Text, pack)
import qualified Data.Text as T

-- import qualified Data.Attoparsec.Text.Lazy as AP
import Data.Attoparsec.Text.Lazy

import qualified Data.Map as M
import Data.Map (Map)

import Control.Applicative

import Data.Monoid ((<>))


data TTree = TTree {
    className :: String,
    dataMembers :: Map String String
}

type ClassMember = (String, String)

cppWord :: Parser String
cppWord = do
            x <- letter <|> char '_'
            xs <- many (letter <|> char '_')
            skipSpace

            return $ x:xs

templType :: Parser String
templType = do
                _ <- char '<'
                skipSpace
                t <- cppType
                skipSpace
                _ <- char '>'
                skipSpace

                return ('<' : t ++ " >")

cppType :: Parser String
cppType = do
            x <- letter <|> char '_'
            xs <- many (letter <|> digit <|> char '_')
            skipSpace
            t <- option "" templType
            skipSpace
            s <- many $ choice [(char '*') <* skipSpace, (char '&') <* skipSpace]
            skipSpace

            return . concat $ [x:xs, t, s]

cppVar :: Parser String
cppVar = cppWord

memberDecl :: Parser ClassMember
memberDecl = (,) <$> cppType <*> cppVar

cppComment :: Parser Text
cppComment = do
    _ <- string "//" 
    takeTill (== '\n')

cComment :: Parser Text
cComment = do
    _ <- string "/*"
    pack <$> manyTill anyChar (string "*/")

comment :: Parser Text
comment = choice [cppComment, cComment]
