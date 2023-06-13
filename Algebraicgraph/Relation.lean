import Lean.Data.HashSet
import Std.Data.List.Basic
import Algebraicgraph.Algebra
open Lean
open Lean.HashSet

structure Relation (α : Type) where
  -- domain
  dom [BEq α] [Hashable α] : HashSet α
  -- relation
  rel [BEq α] [Hashable α] : HashSet (α × α)

instance [ToString α] [BEq α] [Hashable α] : ToString (Relation α) where
  toString x := s!"{x.dom.toList} {x.rel.toList}"

instance [BEq α] [Hashable α] : BEq (Relation α) where
  beq x y := x.dom.toArray == y.dom.toArray && x.rel.toArray == y.rel.toArray

instance : Graph Relation where
  empty := ⟨ empty, empty ⟩
  vertex x := ⟨ empty.insert x, empty ⟩
  overlay x y := ⟨ x.dom.insertMany y.dom, x.rel.insertMany y.rel ⟩
  connect x y :=
    ⟨ x.dom.insertMany y.dom,
      (x.rel.insertMany y.rel).insertMany
        (x.dom.toList.product y.dom.toList)
    ⟩

#check Relation

#eval ((vertex 1) ⇒ (vertex 2) : Relation Nat)
#eval ((vertex 1) ⇒ (vertex 2) ⊕ (vertex 2) ⇒ (vertex 3) : Relation Nat)
#eval ((vertex 1) ⇒ ((vertex 2) ⊕ (vertex 3)) : Relation Nat)

private def pentagon : Relation Nat := circuit (List.range 5)

#eval (path (List.range 5) : Relation Nat)
#eval pentagon
#eval (subgraph_of? (path (List.range 5)) pentagon)
