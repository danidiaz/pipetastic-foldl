{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where 

import Test.Tasty
import Test.Tasty.HUnit

import Data.Functor.Identity
import Data.Monoid
import Data.Foldable
import Data.ByteString
import qualified Control.Foldl as L
import qualified Data.Attoparsec.Text as A
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Free
import Pipes
import qualified Pipes.ByteString as B
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as P
import qualified Pipes.Attoparsec as P
import qualified Pipes.Aeson as P
import qualified Pipes.Text as T
import qualified Pipes.Text.Encoding as T
import qualified Pipes.Text.IO as T
import System.IO
import System.IO.Error
import System.Exit

import Control.Foldl.Pipes


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
            [ test1
            , test2
            , test3
            , test4
            ]

-------------------------------------------------------------------------------
--
adapt p = do
    r <- P.parse p
    return $ case r of
        Just (Right r') -> Right r'
        _ -> Left "parse error"

parser1 :: Char -> A.Parser [Char] 
parser1 c = fmap mconcat $ 
    many (A.notChar c) *> A.many1 (some (A.char c) <* many (A.notChar c))

parserFold1 = fromParser (adapt (parser1 'a'))


parser2 :: Char -> A.Parser [Char] 
parser2 c = fmap mconcat $ 
    many (A.notChar c) *> A.many1 (some (A.char c))

parserFold2 = fromParser (adapt (parser2 'a'))


parser3 :: Char -> A.Parser [Char] 
parser3 c = fmap mconcat $ 
    many (A.notChar c) *> A.many1 (some (A.char c)) <* A.endOfInput

parserFold3 = fromParser (adapt (parser3 'a'))

source :: [T.Text]
source = [ "xx", "xxxxx", "x", 
           "aaaaa" , "aa" , "aaaa",
           "bbbb", "bbbbbb", "bbbbbbb" ]
test1 :: TestTree
test1 = testCase "consumes all" $ do
    assertEqual "" (L.foldM parserFold1 source) 
                   (ExceptT (Identity (Right "aaaaaaaaaaa")))


test2 :: TestTree
test2 = testCase "early finish" $ do
    assertEqual "" (L.foldM parserFold2 source) 
                   (ExceptT (Identity (Right "aaaaaaaaaaa")))

test3 :: TestTree
test3 = testCase "parse error" $ do
    assertEqual "" (L.foldM parserFold3 source) 
                   (ExceptT (Identity (Left "parse error")))


-------------------------------------------------------------------------------

drawTwo :: MonadIO m => P.Parser Char m (Either String (Char,Char))
drawTwo = do
    mx <- P.draw
    liftIO (print mx)
    case mx of
       Just 'x' -> return (Left "early exit") 
       _ -> do
           my <- P.draw
           liftIO (print my)
           return $ case (mx,my) of
               (Just 'a', Just 'b') -> Right ('a','b') 
               _ -> Left "oops"
  
drawTwoFold :: MonadIO m => L.FoldM (ExceptT String m) Char (Char,Char) 
drawTwoFold = fromParserIO drawTwo

charProd :: MonadIO m => Producer Char m ()
charProd = yield 'a' >> yield 'b' >> yield 'c' >> yield 'd'

test4 :: TestTree
test4 = testCase "effectul parser" $ do
    r <- runExceptT (L.impurely P.foldM drawTwoFold charProd)
    assertEqual "" r (Right ('a','b'))
