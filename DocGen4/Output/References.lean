import DocGen4.Output.Template

open Lean

namespace DocGen4

private def proc (args : IO.Process.SpawnArgs) : IO Unit := do
  let child ← IO.Process.spawn args
  let exitCode ← child.wait
  if exitCode != 0 then
    throw (IO.userError s!"external command '{args.cmd}' exited with code {exitCode}")

/-- Get the array of cite keys from bibtexml contents. -/
def getCitekeys (contents : String) : Except String (Array String) :=
  match Xml.parse contents with
  | .ok (.Element tag _ arr) =>
    if tag != "bibtex:file" then
      .error s!"node is '{tag}', but 'bibtex:file' expected"
    else
      let arr2 : Array (Except String String) := arr.map fun node =>
        match node with
        | .Element (.Element tag attr _) =>
          if tag != "bibtex:entry" then
            .error s!"node is '{tag}', but 'bibtex:entry' expected"
          else
            match attr.find? "id" with
            | .some s => .ok s
            | .none => .error "failed to find 'id' attribute"
        | _ => .ok ""
      match arr2.findSome? (fun x => match x with | .error s => .some s | _ => .none) with
      | .some s => .error s
      | .none => .ok (arr2.map (fun x => match x with | .ok s => s | _ => unreachable!)
        |>.filter fun s => not s.trim.isEmpty)
  | .error err => .error err

/-- Preprocess and save the bib file to the output path. -/
def preprocessBibFile (contents : String) : IO Unit := do
  -- create directories
  IO.FS.createDirAll Output.basePath
  IO.FS.createDirAll Output.declarationsBasePath
  -- erase all files
  IO.FS.writeFile (Output.basePath / "references.bib") contents
  IO.FS.writeFile (Output.basePath / "references.xml.tmp") ""
  IO.FS.writeFile (Output.basePath / "references.html.tmp") ""
  IO.FS.writeFile (Output.declarationsBasePath / "citekey.txt") ""
  -- if contents is empty, just do nothing
  if contents.trim.isEmpty then
    return
  -- run `pybtex-convert`
  proc {
    cmd := "pybtex-convert"
    args := #[
      "-f", "bibtex", "-t", "bibtexml", "--preserve-case",
      (Output.basePath / "references.bib").toString,
      (Output.basePath / "references.xml.tmp").toString
    ]
  }
  -- parse the returned XML file
  match getCitekeys (← IO.FS.readFile (Output.basePath / "references.xml.tmp")) with
  | .ok ret =>
    IO.FS.writeFile (Output.declarationsBasePath / "citekey.txt") ("\n".intercalate ret.toList)
  | .error err =>
    throw (IO.userError s!"failed to parse '{Output.basePath / "references.xml.tmp"}': {err}")
  -- run `pybtex-format`
  proc {
    cmd := "pybtex-format"
    args := #[
      "-f", "bibtex", "-b", "html", "--label-style=alpha",
      (Output.basePath / "references.bib").toString,
      (Output.basePath / "references.html.tmp").toString
    ]
  }

namespace Output

private partial def naiveFindSubstr (it : String.Iterator) (p : String) : Option String.Iterator :=
  if it.atEnd then
    .none
  else if it.1.substrEq it.2 p 0 p.utf8ByteSize then
    .some it
  else
    naiveFindSubstr it.next p

private partial def extractItemsAux (arr : Array Xml.Content)
    (s : String) (i : Nat) (ret : Array Html) : Array Html :=
  if h : i < arr.size then
    let new : Array Html :=
      if let .Element (.Element name _ contents) := arr.get ⟨i, h⟩ then
        if name == s then
          ret.push <| .raw <| String.join (contents.map Xml.cToStringEscaped).toList
        else
          ret
      else
        ret
    extractItemsAux arr s (i + 1) new
  else
    ret

private def extractItems (htmlTmp : String) : Array (Html × Html) :=
  match naiveFindSubstr htmlTmp.mkIterator "<dl>" with
  | .none => #[]
  | .some lps =>
    match naiveFindSubstr lps "</dl>" with
    | .none => #[]
    | .some lpe =>
      match Xml.parse (Substring.toString ⟨htmlTmp, lps.2, ⟨lpe.2.1 + 5⟩⟩) with
      | .ok (.Element _ _ arr) =>
        (extractItemsAux arr "dt" 0 #[]).zip (extractItemsAux arr "dd" 0 #[])
      | .error _ => #[]

open scoped DocGen4.Jsx

def refItem (ref : String × Html × Html) : BaseHtmlM Html := do
  pure <|
    <li id={s!"ref_{ref.1}"}>
      <a href={s!"#ref_{ref.1}"}>{.raw "["}{ref.2.1}{.raw "]"}</a>
      {.raw " "}{ref.2.2}
    </li>

def references
    (htmlTmp : String)
    : BaseHtmlM Html := templateLiftExtends (baseHtml "References") do
  let refs := (← read).citekeys.zip (extractItems htmlTmp)
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
