import BibtexQuery.Parser

def main (args : List String) : IO Unit := do
  match args with
  | [input, output] =>
    match BibtexQuery.Parser.bibtexFile (← IO.FS.readFile input).iter with
    | .success _ ret =>
      pure ()
    | .error it err =>
      IO.println s!"error: failed to parse file '{input}' at pos {it.1}: {err}"
  | _ =>
    IO.println "usage: test <input file> <output file>"
