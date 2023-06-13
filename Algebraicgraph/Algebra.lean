-- Andrey Mokhov, Algebraic Graphs with Class (Functional Pearl)
-- https://dl.acm.org/doi/pdf/10.1145/3122955.3122956
import Std.Data.List.Basic

-- the different between connect and overlay can be done by this example:
--
-- Context: g1 is a graph has only vertex 1, and g2 has only vertex 2
--
-- 1. overlay: g1 + g2 then we have two vertex 1 and 2, but has no edge
-- 2. connect: g1 -> g2 then we have two vertex 1 and 2, and has an edge from 1 to 2
class Graph (g : Type _ → Type _) where
  empty : g v
  vertex : v → g v
  overlay : g v → g v → g v
  connect : g v → g v → g v

def empty [i : Graph g] : g v := i.empty
def vertex [i : Graph g] : v → g v := i.vertex
def overlay [i : Graph g] : g v → g v → g v := i.overlay
def connect [i : Graph g] : g v → g v → g v := i.connect

notation:65 x " ⊕ " y => overlay x y
notation:66 x " ⇒ " y => connect x y

def edge [Graph g] (x y : v) : g v :=
  connect (vertex x) (vertex y)

def vertices [Graph g] (l : List v) : g v :=
  (l.map vertex).foldr overlay empty

def clique [Graph g] (l : List v) : g v :=
  (l.map vertex).foldr connect empty

def edges [Graph g] (es : List (v × v)) : g v :=
  (es.map (fun (a, b) => edge a b)).foldr overlay empty

def graph [Graph g] (vs : List v) (es : List (v × v)) : g v :=
  overlay (vertices vs) (edges es)

axiom identity {g : Type → Type} {v : Type} [Graph g] (x : g v)
  : (x ⊕ empty) = x
axiom idempotence {g : Type → Type} {v : Type} [Graph g] (x : g v)
  : (x ⊕ x) = x
axiom absorption {g : Type → Type} {v : Type} [Graph g] (x y : g v)
  : (x ⇒ y ⊕ x ⊕ y) = (x ⇒ y)
axiom saturation {g : Type → Type} {v : Type} [Graph g] (x : g v)
  : (x ⇒ x ⇒ x) = (x ⇒ x)

def subgraph_of? [Graph g] [BEq (g v)] (x y : g v) : Bool :=
  overlay x y == y

def path [Graph g] : List v → g v
  | [] => empty
  | [x] => vertex x
  | xs => edges (xs.zip xs.tail)

def circuit [Graph g] : List v → g v
  | [] => empty
  | x :: xs => path ((x :: xs) ++ [x])

def star [Graph g] (x : v) (ys : List v) : g v :=
  connect (vertex x) (vertices ys)
