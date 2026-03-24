import Lott
import Lott.Elab.Nat

namespace LottExamples.LeanEscape

nonterminal Term, e :=
  | "unit"       : unit
  | "wrap(" e ")" : wrap

nonterminal Ty, τ :=
  | "Top" : top

nonterminal Env, Γ :=
  | "mk " type(Nat -> Term) : mk

judgement_syntax e " : " τ : HasType

judgement HasType := fun e τ => e = [[unit]] ∧ τ = [[Top]]

def wrappedUnit : Term := [[lean% Term.wrap Term.unit]]
def trivialEnv : Env := [[mk [[lean% (fun _ => Term.unit)]]]]

example : [[unit : [[lean% Ty.top]]]] := by
  exact And.intro rfl rfl

example : wrappedUnit = Term.wrap Term.unit := rfl
example : trivialEnv = Env.mk (fun _ => Term.unit) := rfl

nonterminal MyList, ρ :=
  | "mk " type(List Term) : «mk»
  | "append(" ρ ", " ρ' ")" : app

judgement_syntax ρ "is of length" n : MyListLength
judgement MyListLength where

───────────────────────────────── base
[[lean% .mk .nil]] is of length 0

ρ is of length n
ρ' is of length n
───────────────────────────────── app
append(ρ, ρ') is of length n

end LottExamples.LeanEscape
