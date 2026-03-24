import Lott

namespace LottExamples.LeanEscape

nonterminal Term, e :=
  | "unit"       : unit
  | "wrap(" e ")" : wrap

nonterminal Ty, τ :=
  | "Top" : top

judgement_syntax e " : " τ : HasType

judgement HasType := fun e τ => e = [[unit]] ∧ τ = [[Top]]

def wrappedUnit : Term := [[lean% Term.wrap Term.unit]]

example : [[unit : [[lean% Ty.top]]]] := by
  exact And.intro rfl rfl

example : wrappedUnit = Term.wrap Term.unit := rfl

end LottExamples.LeanEscape
