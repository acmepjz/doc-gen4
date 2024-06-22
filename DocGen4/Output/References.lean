import DocGen4.Output.Template

/-!

# Generic functions for references support

This file contains functions for references support,
independent of actual implementation.

The main function is `DocGen4.preprocessBibFile` which preprocess
the contents of bib file using user provided `process` function,
and save the bib file and processed json file to output directory.

Currently we have an implementation of `process` function using `pybtex`,
which is `DocGen4.Pybtex.process`. In the future this may be replaced by
a pure Lean implementation.

---

### Test reference support

It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]

* [Orzech, Morris. *Onto endomorphisms are isomorphisms*][orzech1971]
* [Djoković, D. Ž. *Epimorphisms of modules which must be isomorphisms*][djokovic1973]
* [Ribenboim, Paulo. *Épimorphismes de modules qui sont nécessairement
  des isomorphismes*][ribenboim1971]

-/

open Lean DocGen4 Output

namespace DocGen4

/-- Test reference support:
It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]
-/
structure TestStructure where
  /-- Test reference support:
  It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
  Ribenboim [ribenboim1971]
  -/
  test1 : Nat
  /-- Test reference support:
  It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
  Ribenboim [ribenboim1971]
  -/
  test2 : Nat → Nat

/-- Test reference support:
It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]
-/
inductive TestInductive
/-- Test reference support:
It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]
-/
| test1 : TestInductive
/-- Test reference support:
It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]
-/
| test2 : Nat → TestInductive
/-- Test reference support:
It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]
-/
| test3 : Nat → Nat → TestInductive
/-- Test reference support:
It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]
-/
| test4 : (Nat → Nat) → TestInductive

/-- Preprocess (using the user provided `process` function)
and save the bib file to the output path.

---

Test reference support:
It was introduced in papers by Orzech [orzech1971], Djoković [djokovic1973] and
Ribenboim [ribenboim1971]

-/
def preprocessBibFile (contents : String) (process : String → IO (Array BibItem)) : IO Unit := do
  -- create directories
  IO.FS.createDirAll basePath
  IO.FS.createDirAll declarationsBasePath
  -- erase all files
  IO.FS.writeFile (basePath / "references.bib") contents
  IO.FS.writeFile (declarationsBasePath / "references.json") "[]"
  -- if contents is empty, just do nothing
  if contents.trim.isEmpty then
    return
  -- run the user provided process function
  let items ← process contents
  -- save the result
  IO.FS.writeFile (declarationsBasePath / "references.json") (toString (toJson items))

namespace Output

open scoped DocGen4.Jsx

def refItem (ref : BibItem) (backrefs : Array BackrefItem) : BaseHtmlM Html := do
  let backrefs := backrefs.filter (fun x => x.citekey == ref.citekey)
  let toHtml (i : Nat) : BaseHtmlM (Array Html) := do
    let .some backref := backrefs.get? i | unreachable!
    let href := s!"{moduleNameToFile "" backref.modName}#_backref_{backref.index}"
    let title := s!"File: {backref.modName}" ++
      if backref.funName.isEmpty then "" else s!"\nLocation: {backref.funName}"
    pure #[.raw " ", <a href={href} title={title}>{.text s!"[{i + 1}]"}</a>]
  let backrefHtml : Html ← (do
    if backrefs.isEmpty then
      pure (.raw "")
    else
      pure <small>[(← (Array.range backrefs.size).mapM toHtml).foldl (· ++ ·) #[]]</small>)
  pure <|
    <li id={s!"ref_{ref.citekey}"}>
      <a href={s!"#ref_{ref.citekey}"}>{.text ref.tag}</a>
      {.raw " "}{.raw ref.html}{backrefHtml}
    </li>

def references (backrefs : Array BackrefItem) :
    BaseHtmlM Html := templateLiftExtends (baseHtml "References") do
  pure <|
    <main>
      <a id="top"></a>
      <h1>References</h1>
      <ul>
      [← (← read).refs.mapM (refItem · backrefs)]
      </ul>
    </main>

end Output

end DocGen4
