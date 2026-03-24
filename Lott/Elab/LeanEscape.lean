import Lott.Elab.Basic

namespace Lott

open Lean
open Lean.Elab
open Lean.Parser
open Lean.Parser.Term

declare_syntax_cat Lott.Symbol.LeanEscape

@[Lott.Symbol_parser]
private
def leanEscape_parser : Parser :=
  leadingNode `Lott.Symbol.LeanEscape Parser.maxPrec <| symbol escapeKeyword >> termParser

@[macro Lott.symbolEmbed]
private
def leanEscapeImpl : Macro := fun
  | .node _ ``Lott.symbolEmbed #[.atom _ "[[", stx, .atom _ "]]"] =>
    match getEscapeTerm? stx with
    | some term => pure term
    | none => Macro.throwUnsupported
  | _ => Macro.throwUnsupported

end Lott
