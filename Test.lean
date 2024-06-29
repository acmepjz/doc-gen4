import BibtexQuery.Parser
import DocGen4.Output.Bibtex.TexDiacritics

open Lean Parsec BibtexQuery DocGen4 Bibtex

def test (category : String) (name : String) (tags : List Tag) : String :=
  let author? : Option String := tags.findSome? fun t =>
    if t.name = "author" then .some t.content else .none
  let title? : Option String := tags.findSome? fun t =>
    if t.name = "title" then .some t.content else .none
  s!"category={category}\ncitekey={name}\nauthor={author?.getD "(author not found)"}\ntitle={title?.getD "(title not found)"}"

def main (args : List String) : IO Unit := do
  match args with
  | [input, output] =>
    match Parser.bibtexFile (← IO.FS.readFile input).iter with
    | .success _ ret =>
      let ret2 : List String := ← ret.filterMapM fun e => do
        if let .normalType category name tags := e then
          let s := test category name tags
          match (texDiacritics <* eof) s.iter with
          | .success _ ret =>
            match (removeBraces <* eof) ret.iter with
            | .success _ ret =>
              return .some ret
            | .error it err =>
              IO.println s!"error: failed to run 'removeBraces' on '{it.1}' at pos {it.2}: {err}"
              return .none
          | .error it err =>
            IO.println s!"error: failed to run 'texDiacritics' on '{it.1}' at pos {it.2}: {err}"
            return .none
        else
          return .none
      IO.FS.writeFile output ("\n".intercalate ret2)
    | .error it err =>
      IO.println s!"error: failed to parse file '{input}' at pos {it.2}: {err}"
  | _ =>
    IO.println "usage: test <input file> <output file>"
