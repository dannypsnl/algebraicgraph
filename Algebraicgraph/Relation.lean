import Std.Data.HashSet
import Std.Data.List.Basic
import Algebraicgraph.Algebra
open Std
open Std.HashSet

structure Relation (α : Type) where
  domain [BEq α] [Hashable α] : HashSet α
  relation [BEq α] [Hashable α] : HashSet (α × α)

instance [ToString α] [BEq α] [Hashable α] : ToString (Relation α) where
  toString x := s!"{x.domain.toList} {x.relation.toList}"

instance [BEq α] [Hashable α] : BEq (Relation α) where
  beq x y := x.domain.toArray == y.domain.toArray && x.relation.toArray == y.relation.toArray

instance : Graph Relation where
  empty := ⟨ empty, empty ⟩
  vertex x := ⟨ empty.insert x, empty ⟩
  overlay x y := ⟨ x.domain.insertMany y.domain, x.relation.insertMany y.relation ⟩
  connect x y :=
    ⟨ x.domain.insertMany y.domain,
      (x.relation.insertMany y.relation).insertMany
        (x.domain.toList.product y.domain.toList)
    ⟩

def Relation.edgeList [BEq v] [Hashable v] (g : Relation v)
  : List (v × v) :=
  g.relation.toList

#check Relation

#eval ((vertex 1) ⇒ (vertex 2) : Relation Nat)
#eval ((vertex 1) ⇒ (vertex 2) ⊕ (vertex 2) ⇒ (vertex 3) : Relation Nat)
#eval ((vertex 1) ⇒ ((vertex 2) ⊕ (vertex 3)) : Relation Nat)

private def pentagon : Relation Nat := circuit (List.range 5)

#eval (path (List.range 5) : Relation Nat)
#eval pentagon
#eval (subgraph_of? (path (List.range 5)) pentagon)
