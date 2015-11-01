module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe (logAny)
import Data.Array (catMaybes, concatMap)
import Data.Either
import Data.List (List(..), fromList)
import Data.Maybe
import Data.Foldable
import Text.Parsing.Parser (runParser, ParseError(..))
import Datalog.Parser
import Datalog.Graph

main :: Eff (console :: CONSOLE) Unit
main = do
  log $ either (const "no graph generated") (toDotty fromTerm) g1E
  where
    atomsE :: Either ParseError (List Atom)
    atomsE = runParser kb2 clauses

    g1E :: Either ParseError (Graph Term String)
    g1E = (fromClauses <<< fromList) <$> atomsE

kb2 :: String
kb2 =
  "module(\"Test1\")." ++
  "module(\"Zoink\")." ++
  "module(\"Fnord\")." ++
  "data(\"Traffic\")." ++
  "newtype(\"Sum\")." ++
  "value(\"f\")." ++
  "data_ctor(\"Red\", \"Traffic\")." ++
  "data_ctor(\"Green\", \"Traffic\")." ++
  "data_ctor(\"Sum\", \"Sum\")." ++
  "defined_in(\"Test1\", \"Zoink\")." ++
  "defined_in(\"Zoink\", \"Fnord\")." ++
  "defined_in(\"Traffic\", \"Test1\")." ++
  "defined_in(\"Sum\", \"Test1\")." ++
  "defined_in(\"f\", \"Test1\")."
