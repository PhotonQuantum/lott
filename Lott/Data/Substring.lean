partial
def Substring.Raw.dropPrefixes (s pre : Substring.Raw) : Substring.Raw :=
  if pre.bsize = 0 then
    s
  else
    match s.dropPrefix? pre with
    | some s' => Substring.Raw.dropPrefixes s' pre
    | none => s
