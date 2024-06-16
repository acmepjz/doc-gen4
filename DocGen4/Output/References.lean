import DocGen4.Output.Template
import Lean.Data.Parsec
import Lean.Data.HashMap

open Lean

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

def parse (s : String) : List (String × HashMap String String) :=
  sorry

end BibParser

open scoped DocGen4.Jsx

def Output.references : BaseHtmlM Html := templateLiftExtends (baseHtml "References") do
  pure <|
    <main>
      <a id="top"></a>
      <h1>References</h1>

      -- TODO:
    </main>

end DocGen4
