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
        m.startLineNumber <- float (sp.Line * 1)
        m.endLineNumber <- float (ep.Line + 1)
        m.startColumn <- float (sp.Col + 1)
        m.endColumn <- float (ep.Col + 1)
        m.message <- msg
        m.severity <- Monaco.Severity.Warning
      )
    for CompilerMsg ({ StartPos = sp; EndPos = ep }, msg) in errors do
      jsOptions<Monaco.Editor.IMarkerData>(fun m ->
        m.startLineNumber <- float (sp.Line * 1)
        m.endLineNumber <- float (ep.Line + 1)
        m.startColumn <- float (sp.Col + 1)
        m.endColumn <- float (ep.Col + 1)
        m.message <- msg
        m.severity <- Monaco.Severity.Error
      )
  |]