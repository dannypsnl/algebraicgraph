-- Andrey Mokhov, Algebraic Graphs with Class (Functional Pearl)
-- https://dl.acm.org/doi/pdf/10.1145/3122955.3122956

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

def edge [i : Graph g] (x y : v) : g v :=
  connect (vertex x) (vertex y)

def vertices [i : Graph g] (l : List v) : g v :=
  List.foldr overlay empty (List.map vertex l)

def clique [i : Graph g] (l : List v) : g v :=
  List.foldr connect empty (List.map vertex l)

def edges [i : Graph g] (es : List (v × v)) : g v :=
  List.foldr overlay empty (List.map (fun (a, b) => edge a b) es)

def graph [i : Graph g] (vs : List v) (es : List (v × v)) : g v :=
  overlay (vertices vs) (edges es)

axiom identity {g : Type → Type} {v : Type} [Graph g] (x : g v)
  : (x ⊕ empty) = x
axiom idempotence {g : Type → Type} {v : Type} [Graph g] (x : g v)
  : (x ⊕ x) = x
axiom absorption {g : Type → Type} {v : Type} [Graph g] (x y : g v)
  : (x ⇒ y ⊕ x ⊕ y) = (x ⇒ y)
axiom saturation {g : Type → Type} {v : Type} [Graph g] (x : g v)
  : (x ⇒ x ⇒ x) = (x ⇒ x)

def subgraphOf? [Graph g] [BEq (g v)] (x y : g v) : Bool :=
  overlay x y == y
