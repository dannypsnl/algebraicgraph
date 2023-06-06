import Lean.Data.HashSet
import Algebraicgraph.Algebra
open Lean

structure Relation (α : Type) where
  -- domain
  dom [BEq α] [Hashable α] : HashSet α
  -- relation
  rel [BEq α] [Hashable α] : HashSet (α × α)

instance [ToString α] [BEq α] [Hashable α] : ToString (Relation α) where
  toString x := s!"{x.dom.toList} {x.rel.toList}"

instance : Graph Relation where
  empty := ⟨ default, default ⟩
  vertex x := ⟨ HashSet.empty.insert x, default ⟩
  overlay x y := ⟨ x.dom.insertMany y.dom, x.rel.insertMany y.rel ⟩
  connect x y :=
    ⟨ x.dom.insertMany y.dom,
      (x.rel.insertMany y.rel).insertMany (x.dom.toList.zip y.dom.toList)
    ⟩

#check Relation

def hi : Relation Nat :=
  connect (vertex 1) (vertex 2)

#eval hi
