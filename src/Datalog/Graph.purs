module Datalog.Graph where

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

-- TODO generalise
data Graph v e = Graph (Array (Vertex v))
                       (Array (Edge v e))

newtype Vertex v = Vertex v

newtype Edge v e = Edge { source :: v, target :: v, label :: e }

-- TODO generify in Graph's 2nd param
toDotty :: forall v. (v -> String) -> Graph v String -> String
toDotty labeler (Graph vertices edges) =
  "digraph G1 {\n" ++
  "\n" ++
  foldMap fromVertex vertices ++
  "\n" ++
  foldMap fromEdge edges ++
  "\n" ++
  "\n}"
  where
    fromVertex (Vertex v) = "todo [shape=box, style=filled, fillcolor=\"/rdbu11/8\"]"
    fromEdge (Edge { source: s, target: t, label: l }) =
      "\"" ++ labeler s ++ "\" -> \"" ++ labeler t ++ "\" [label=\"" ++ l ++ "\"]\n"

--------------------------------------------------------------------------------

type Label = String

fromTerm :: Term -> Label
fromTerm (Var name) = "?" ++ name
fromTerm (Con name) = name

fromAtom :: Atom -> Maybe (Edge Term String)
fromAtom (Pred name [a,b]) = Just (Edge { source: a, target: b, label: name } )
fromAtom (Pred name _    ) = Nothing

fromClauses :: Array Atom -> Graph Term String
fromClauses atoms = Graph vertices edges
  where
    vertices = []
    edges    = catMaybes $ fromAtom <$> atoms
