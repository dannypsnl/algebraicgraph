import Algebraicgraph.Algebra

structure ToList (α : Type) where
  toList : List α
  deriving Repr

instance : Graph ToList where
  empty := ⟨ [] ⟩
  vertex x := ⟨ [x] ⟩
  overlay x y := ⟨ x.toList ++ y.toList ⟩
  connect x y := ⟨ x.toList ++ y.toList ⟩

#eval ((vertex 1) ⇒ (vertex 2) : ToList Nat)
#eval ((vertex 1) ⇒ (vertex 2) ⊕ (vertex 2) ⇒ (vertex 3) : ToList Nat)
#eval ((vertex 1) ⇒ ((vertex 2) ⊕ (vertex 3)) : ToList Nat)

private def pentagon : ToList Nat := circuit (List.range 5)

#eval (path (List.range 5) : ToList Nat)
#eval pentagon
