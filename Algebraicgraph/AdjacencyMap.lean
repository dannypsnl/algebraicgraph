import Std.Data.HashSet
import Std.Data.HashMap
import Algebraicgraph.Algebra
open Std
open Std.HashSet
open Std.HashMap

structure AdjacencyMap (α : Type) where
  adjacencyMap [BEq α] [Hashable α] : HashMap α (HashSet α)

instance [ToString α] [BEq α] [Hashable α] : ToString (AdjacencyMap α) where
  toString x :=
    let xs := x.adjacencyMap.toList
    xs.foldr (fun (x, y) r => r ++ toString x ++ " -> " ++ toString y.toList ++ "\n") ""

def mergeMap [BEq α] [Hashable α] (x y : HashMap α (HashSet α))
  : HashMap α (HashSet α) :=
  x.fold (fun σ k v => σ.insert k (v.insertMany (σ.getD k emptyWithCapacity))) y

def newValues [BEq α] [Hashable α] (x : HashMap α (HashSet α)) (nv : (HashSet α))
  : HashMap α (HashSet α) :=
  x.fold (fun σ k _ => σ.insert k nv) emptyWithCapacity

def keys [BEq α] [Hashable α] (x : HashMap α (HashSet α))
  : HashSet α :=
  x.fold (fun σ k _ => σ.insert k) emptyWithCapacity

instance : Graph AdjacencyMap where
  empty := ⟨ emptyWithCapacity ⟩
  vertex x := ⟨ emptyWithCapacity.insert x emptyWithCapacity ⟩
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
