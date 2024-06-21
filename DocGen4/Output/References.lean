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

/-- Preprocess (using the user provided `process` function)
and save the bib file to the output path. -/
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

def refItem (ref : BibItem) : BaseHtmlM Html := do
  pure <|
    <li id={s!"ref_{ref.citekey}"}>
      <a href={s!"#ref_{ref.citekey}"}>{.text ref.tag}</a>
      {.raw " "}{.raw ref.html}
    </li>

def references : BaseHtmlM Html := templateLiftExtends (baseHtml "References") do
  let config ← read
  pure <|
    <main>
      <a id="top"></a>
      <h1>References</h1>
      <ul>
      [← config.refs.mapM refItem]
      </ul>
    </main>

end Output

end DocGen4
