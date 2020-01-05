module MonacoHelper

open Sanuki
open Sanuki.Compiler
open Monaco

open Fable.Core
open Fable.Core.JsInterop

let gerMarkers (res: CompileMsgResult<Assembly, Ast.Range>) =
  let warns, errors =
    match res with
    | COk (_, warns) -> warns, []
    | CError (warns, errors) -> warns, errors

  [| 
    for CompilerMsg ({ StartPos = sp; EndPos = ep }, msg) in warns do
      jsOptions<Monaco.Editor.IMarkerData>(fun m ->
        m.startLineNumber <- float (sp.Line + 1)
        m.endLineNumber <- float (ep.Line + 1)
        m.startColumn <- float (sp.Col + 1)
        m.endColumn <- float (ep.Col + 1)
        m.message <- msg
        m.severity <- Monaco.Severity.Warning
      )
    for CompilerMsg ({ StartPos = sp; EndPos = ep }, msg) in errors do
      jsOptions<Monaco.Editor.IMarkerData>(fun m ->
        m.startLineNumber <- float (sp.Line + 1)
        m.endLineNumber <- float (ep.Line + 1)
        m.startColumn <- float (sp.Col + 1)
        m.endColumn <- float (ep.Col + 1)
        m.message <- msg
        m.severity <- Monaco.Severity.Error
      )
  |]

let setup () =
  printfn "setup"
  Monaco.languages.register <|
    jsOptions<Monaco.Languages.ILanguageExtensionPoint> (fun obj ->
      obj.id <- "sanuki"
    )

  let rule (regex: string) include_ action =
    jsOptions<Monaco.Languages.IMonarchLanguageRule> (fun obj ->
      obj.regex <- Some (!^ regex)
      obj.``include`` <- include_
      obj.action <-
        Some (jsOptions<Monaco.Languages.IMonarchLanguageAction> action)
    )
  
  Monaco.languages.setMonarchTokensProvider(
    "sanuki",
    jsOptions<Monaco.Languages.IMonarchLanguage> (fun obj ->
      obj.tokenizer <- jsOptions<Monaco.Languages.TypeLiteral_08> (fun obj ->
        obj.Item "root" <- ResizeArray [|
          rule
            "/(let)|(goto((_indirect)|(_if_false))?)|(call)|(label)|(set)|(exit)|(push)|(pop)|(copy)/"
            None
            (fun obj -> obj.token <- Some "keyword")
          rule "/(pub)|(sync(\\[.+\\])?(<.+>)?)/" None (fun obj -> obj.token <- Some "keyword")
          rule "/:(_|[a-zA-Z0-9])+/" None (fun obj -> obj.token <- Some "type.identifier")
          rule "/#.+$/" None (fun obj -> obj.token <- Some "comment")
        |]
      )
    )
  )