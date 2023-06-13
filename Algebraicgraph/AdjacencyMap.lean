import Lean.Data.HashSet
import Lean.Data.HashMap
import Algebraicgraph.Algebra
open Lean
open Lean.HashSet
open Lean.HashMap

structure AdjacencyMap (α : Type) where
  adjacencyMap [BEq α] [Hashable α] : HashMap α (HashSet α)

instance [ToString α] [BEq α] [Hashable α] : ToString (AdjacencyMap α) where
  toString x :=
    let xs := x.adjacencyMap.toList
    xs.foldr (fun (x, y) r => r ++ toString x ++ " -> " ++ toString y.toList ++ "\n") ""

def mergeMap [BEq α] [Hashable α] (x y : HashMap α (HashSet α))
  : HashMap α (HashSet α) :=
  x.fold (fun σ k v => σ.insert k (v.insertMany (σ.findD k empty))) y

def newValues [BEq α] [Hashable α] (x : HashMap α (HashSet α)) (nv : (HashSet α))
  : HashMap α (HashSet α) :=
  x.fold (fun σ k _ => σ.insert k nv) empty

def keys [BEq α] [Hashable α] (x : HashMap α (HashSet α))
  : HashSet α :=
  x.fold (fun σ k _ => σ.insert k) empty

instance : Graph AdjacencyMap where
  empty := ⟨ empty ⟩
  vertex x := ⟨ empty.insert x empty ⟩
  overlay x y := ⟨ mergeMap x.adjacencyMap y.adjacencyMap ⟩
  connect (x y : AdjacencyMap _) :=
    ⟨ mergeMap x.adjacencyMap y.adjacencyMap
      |> mergeMap (newValues x.adjacencyMap (keys y.adjacencyMap))
    ⟩

#eval ((vertex 1) ⇒ (vertex 2) : AdjacencyMap Nat)
#eval ((vertex 1) ⇒ (vertex 2) ⊕ (vertex 2) ⇒ (vertex 3) : AdjacencyMap Nat)
#eval ((vertex 1) ⇒ ((vertex 2) ⊕ (vertex 3)) : AdjacencyMap Nat)

private def pentagon : AdjacencyMap Nat := circuit (List.range 5)

#eval (path (List.range 5) : AdjacencyMap Nat)
#eval pentagon
