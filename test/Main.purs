module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe (logAny)
import Data.Either
import Data.List (List(..), fromList)
import Text.Parsing.Parser (runParser, ParseError(..))
import Datalog.Parser
import Datalog.Graph

main :: Eff (console :: CONSOLE) Unit
main = do
  let
    atomsE :: Either ParseError (List Atom)
    atomsE = runParser "p1(a,b). p1(  b  ,  X ).   p3(d,e,f)." clauses

    g1 :: Either ParseError (Graph Term String)
    g1 = (fromClauses <<< fromList) <$> atomsE

  log    $ show atomsE
  logAny $ g1
  logAny $ toDotty fromTerm <$> g1
  log    $ either (const "# no graph generated") (toDotty fromTerm) g1
