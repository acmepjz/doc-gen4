import BibtexQuery.Parser
import BibtexQuery.Name

open Lean Parsec BibtexQuery TexDiacritics

def test (category : String) (name : String) (tags : List Tag) : String :=
  let author : String := tags.findSome? (fun t =>
    if t.name = "author" then .some t.content else .none) |>.getD ""
  let title : String := tags.findSome? (fun t =>
    if t.name = "title" then .some t.content else .none) |>.getD ""
  let abbr : String :=
    match processNames author with
    | .ok arr =>
      String.join (arr.map (fun x =>
        if arr.size ≥ 2 then x.oneLetterAbbr else x.threeLetterAbbr)).toList
    | .error err => err
  s!"category={category}\ncitekey={name}\nauthor={author}\nabbr={abbr}\ntitle={title}"

def main (args : List String) : IO Unit := do
  match args with
  | [input, output] =>
    match Parser.bibtexFile (← IO.FS.readFile input).iter with
    | .success _ ret =>
      let ret2 : List String := ← ret.filterMapM fun e => do
        if let .normalType category name tags := e then
          let s := test category name tags
          match texDiacritics s.iter with
          | .success _ s =>
            match removeBraces s.iter with
            | .success _ s =>
              return .some s
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
