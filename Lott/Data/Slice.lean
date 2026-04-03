partial
def String.Slice.dropPrefixes (s pre : String.Slice) : String.Slice :=
  if pre.isEmpty then
    s
  else
    match s.dropPrefix? pre with
    | some s' => String.Slice.dropPrefixes s' pre
    | none => s
