import DocGen4.Output.Template
import Lean.Data.Parsec
import Lean.Data.HashMap

open Lean Parsec

namespace DocGen4

namespace BibParser

private inductive RemoveCommentState
| normal : RemoveCommentState
| backslash : RemoveCommentState
| comment : RemoveCommentState
| commentAfterLinebreak : RemoveCommentState

private partial def removeCommentAux (s : String) (i : String.Pos)
    (state : RemoveCommentState) (r : String) : String :=
  if s.atEnd i then
    r
  else
    let c := s.get i
    let j := s.next i
    match state with
    | .normal =>
      if c == '#' then removeCommentAux s j .comment r
      else if c == '\\' then removeCommentAux s j .backslash (r.push c)
      else removeCommentAux s j .normal (r.push c)
    | .backslash => removeCommentAux s j .normal (r.push c)
    | .comment =>
      if c == '\n' then removeCommentAux s j .commentAfterLinebreak (r.push c)
      else removeCommentAux s j .comment r
    | .commentAfterLinebreak =>
      if c == ' ' || c == '\t' then removeCommentAux s j .commentAfterLinebreak r
      else removeCommentAux s i .normal r

def removeComment (s : String) : String := removeCommentAux s 0 .normal ""

private def citeKey : Parsec String := ws *> many1Chars
  (satisfy fun c => ('0' ≤ c ∧ c ≤ '9') ∨ ('A' ≤ c ∧ c ≤ 'Z') ∨ ('a' ≤ c ∧ c ≤ 'z') ∨
    c = '-' ∨ c = '_' ∨ c = ':' ∨ c = '+') <* ws

private def unquoteString : Parsec String := many1Chars
  (satisfy fun c => c ≠ '\r' ∧ c ≠ '\n' ∧ c ≠ ' ' ∧ c ≠ '\t' ∧ c ≠ '\'' ∧ c ≠ '\"' ∧ c ≠ ',' ∧ c ≠ '{' ∧ c ≠ '}')

private def quoteString : Parsec String := pchar '\"' *> manyChars
  (satisfy fun c => c ≠ '\"') <* pchar '\"'

private partial def curlyBraceStringAux (it : String.Iterator)
    (level : Nat) (isBackslash : Bool) (r : String) : Bool × String.Pos × String :=
  if it.atEnd then
    if level == 0 then
      (true, it.2, r)
    else
      (false, it.2, "'}' expected")
  else
    let c := it.curr
    let next := it.next
    if isBackslash then
      curlyBraceStringAux next level false (r.push c)
    else if c == '\\' then
      curlyBraceStringAux next level true (r.push c)
    else if c == '{' then
      curlyBraceStringAux next (level + 1) false (r.push c)
    else if c == '}' then
      if level == 0 then
        (true, it.2, r)
      else
        curlyBraceStringAux next (level - 1) false (r.push c)
    else
      curlyBraceStringAux next level false (r.push c)

private def curlyBraceString : Parsec String := pchar '{' *> (fun it =>
  let ret := curlyBraceStringAux it 0 false ""
  if ret.1 then .success ⟨it.1, ret.2.1⟩ ret.2.2
  else .error ⟨it.1, ret.2.1⟩ ret.2.2) <* pchar '}'

private def keyValuePair : Parsec (String × String) := attempt do
  let key ← citeKey <* pchar '='
  let value ← ws *> (curlyBraceString <|> quoteString <|> unquoteString) <* ws
  return (key.toLower, value)

private def bibEntry : Parsec (String × HashMap String String) := attempt do
  let kind := (← ws *> pchar '@' *> citeKey <* pchar '{').toLower
  let citekey ← citeKey
  let keyValues ← many (pchar ',' *> keyValuePair) <* pchar '}'
  return (citekey, HashMap.ofList keyValues.toList
    |>.insert "__kind__" kind
    |>.insert "__citekey__" citekey)

def parse (s : String) : Except String (Array (String × HashMap String String)) :=
  match many bibEntry (removeComment s).mkIterator with
  | .success _ ret => .ok ret
  | .error _ s => .error s

end BibParser

/-- Preprocess and save the bib file to the output path. -/
def preprocessBibFile (contents : String) : IO Unit := do
  IO.FS.createDirAll Output.basePath <|> pure ()
  IO.FS.createDirAll Output.declarationsBasePath <|> pure ()
  match BibParser.parse contents with
  | .ok ret =>
    IO.println s!"INFO: processed {ret.size} bib entries"
    IO.FS.writeFile (Output.basePath / "references.bib") contents
    IO.FS.writeFile (Output.declarationsBasePath / "citekey.txt")
      ("\n".intercalate (ret.map (·.1)).toList)
  | .error msg =>
    IO.println s!"ERROR: failed to parse bib file, error message: {msg}"
    IO.FS.writeFile (Output.basePath / "references.bib") ""
    IO.FS.writeFile (Output.declarationsBasePath / "citekey.txt") ""

namespace Output

open scoped DocGen4.Jsx

def refItem (ref : String × HashMap String String) : BaseHtmlM Html := do
  pure <|
    <li id={s!"ref_{ref.1}"}>
      <a href={s!"#ref_{ref.1}"}>{s!"[{ref.1}]"}</a>
      {" " ++ (ref.2.findD "author" "(author not found)") ++ ". "}
      <i>{ref.2.findD "title" "(title not found)" ++ "."}</i>
    </li>

def references
    (refs : Array (String × HashMap String String))
    : BaseHtmlM Html := templateLiftExtends (baseHtml "References") do
  pure <|
    <main>
      <a id="top"></a>
      <h1>References</h1>
      <ul>
      [← refs.mapM refItem]
      </ul>
    </main>

end Output

end DocGen4
