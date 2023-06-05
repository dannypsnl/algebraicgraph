-- Andrey Mokhov, Algebraic Graphs with Class (Functional Pearl)
-- https://dl.acm.org/doi/pdf/10.1145/3122955.3122956

-- the different between connect and overlay can be done by this example:
--
-- Context: g1 is a graph has only vertex 1, and g2 has only vertex 2
--
-- 1. overlay: g1 + g2 then we have two vertex 1 and 2, but has no edge
-- 2. connect: g1 -> g2 then we have two vertex 1 and 2, and has an edge from 1 to 2
class Graph (g : Type) where
  Vertex : Type → Type
  empty : g
  vertex : Vertex g → g
  overlay : g → g → g
  connect : g → g → g

def empty [i : Graph g] : g := i.empty
def vertex [i : Graph g] : i.Vertex g → g := i.vertex
def overlay [i : Graph g] : g → g → g := i.overlay
def connect [i : Graph g] : g → g → g := i.connect

notation x " ⊕ " y => overlay x y
notation x " ⇒ " y => connect x y

def edge [i : Graph g] (x y : i.Vertex g) : g :=
  connect (vertex x) (vertex y)

def vertices [i : Graph g] (l : List (i.Vertex g)) : g :=
  List.foldr overlay empty (List.map vertex l)

def clique [i : Graph g] (l : List (i.Vertex g)) : g :=
  List.foldr connect empty (List.map vertex l)

def edges [i : Graph g] (es : List (i.Vertex g × i.Vertex g)) : g :=
  List.foldr overlay empty (List.map (fun (a, b) => edge a b) es)

def graph [i : Graph g] (vs : List (i.Vertex g)) (es : List (i.Vertex g × i.Vertex g)) : g :=
  overlay (vertices vs) (edges es)

axiom identity {g : Type} [Graph g] (x : g) : (x ⊕ empty) = x
axiom idempotence {g : Type} [Graph g] (x : g) : (x ⊕ x) = x
axiom absorption {g : Type} [Graph g] (x y : g) : (x ⇒ y ⊕ x ⊕ y) = (x ⇒ y)
axiom saturation {g : Type} [Graph g] (x : g) : (x ⇒ x ⇒ x) = (x ⇒ x)
