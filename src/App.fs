module App.View

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fulma
open Monaco
open Browser
open Browser.Types

open Sanuki
open Sanuki.Compiler

type ISavedState =
  abstract code: string

let private loadState(_key: string): ISavedState = importMember "./js/util.js"
let private updateQuery(_code : string): unit = importMember "./js/util.js"

type IEditor = Monaco.Editor.IStandaloneCodeEditor

type State = Loading | Idle | Compiling | Compiled

type CompileResult = CompileMsgResult<Assembly, Ast.Range>

type Model = {
  Editor: IEditor
  State: State
  Code : string
  Result: string
  Errors: Monaco.Editor.IMarkerData[]
  IsMainPanelExpanded: bool
  IsProblemsPanelExpanded: bool
  PanelSplitRatio: float
}

type Msg =
  | SetEditor of IEditor
  | LoadSuccess
  | LoadFail
  | ParseEditorCode
  | StartCompile
  | EndCompile of CompileResult
  | ToggleMainPanel
  | ToggleProblemsPanel
  | ChangeCode of string
  | UrlHashChange
  | Nop

let compileCmd src : Cmd<Msg> =
  let result = parseAndCompile src
  Cmd.OfFunc.result (EndCompile result)

let update (msg: Msg) (model: Model) =
  match msg with
  | Nop -> model, Cmd.none
  | LoadSuccess ->
    let activateParsing dispatch =
      let obs =
        createObservable (fun trigger ->
          model.Editor.getModel().onDidChangeContent(fun _ -> trigger()) |> ignore
        )
      debounce 500 obs |> Observable.add (fun () -> dispatch ParseEditorCode)
      obs.Trigger()
    { model with State = Idle },
    Cmd.batch [ [ activateParsing ]; Cmd.ofMsg StartCompile ]
  | LoadFail ->
    printfn "load failed"
    { model with State = Idle }, Cmd.none
  | ParseEditorCode ->
    let content =
      model.Editor.getModel().getValue(Monaco.Editor.EndOfLinePreference.TextDefined, true)
    model, Cmd.ofMsg (ChangeCode content)
  | SetEditor ed -> { model with Editor = ed }, Cmd.ofMsg LoadSuccess
  | ToggleMainPanel ->
    { model with IsMainPanelExpanded = not model.IsMainPanelExpanded }, Cmd.none
  | ToggleProblemsPanel ->
    { model with IsProblemsPanelExpanded = not model.IsProblemsPanelExpanded }, Cmd.none
  | StartCompile ->
    match model.State with
    | Loading | Compiling -> model, Cmd.none
    | Compiled | Idle ->
      { model with State = Compiling }, compileCmd model.Code
  | EndCompile result ->
    let markers = MonacoHelper.gerMarkers result
    match result with
    | COk (asm, _) ->
      { model with State = Compiled; Result = Assembly.toUAssembler asm; Errors = markers },
      Cmd.OfFunc.either updateQuery model.Code (fun _ -> Nop) (fun _ -> Nop)
    | CError _ ->
      { model with State = Compiled; Errors = markers }, Cmd.none
  | ChangeCode code -> { model with Code = code }, Cmd.ofMsg StartCompile
  | UrlHashChange ->
    let parsed = loadState(Literals.STORAGE_KEY)
    model,
    if System.String.IsNullOrEmpty parsed.code then Cmd.none else Cmd.ofMsg (ChangeCode parsed.code)

let init () =
  let saved = loadState(Literals.STORAGE_KEY)
  MonacoHelper.setup() |> ignore
  {
    State = Loading
    Editor = Unchecked.defaultof<IEditor>
    Code = if System.String.IsNullOrEmpty saved.code then Sanuki.Example else saved.code
    Errors = [||]
    Result = ""
    IsMainPanelExpanded = false
    IsProblemsPanelExpanded = true
    PanelSplitRatio = 0.5
  }, Cmd.none

let private editorOptions (fontSize : float) (fontFamily : string) =
  jsOptions<Monaco.Editor.IEditorConstructionOptions>(fun o ->
    let minimapOptions = jsOptions<Monaco.Editor.IEditorMinimapOptions>(fun oMinimap ->
        oMinimap.enabled <- Some false
    )
    o.language <- Some "sanuki"
    o.fontSize <- Some fontSize
    o.theme <- Some "vs-dark"
    o.minimap <- Some minimapOptions
    o.fontFamily <- Some fontFamily
    o.fontLigatures <- Some (fontFamily = "Fira Code")
    o.fixedOverflowWidgets <- Some true
  )

let private problemsPanel (isExpanded : bool) (errors : Monaco.Editor.IMarkerData[]) dispatch =
  let bodyDisplay = if isExpanded then "" else "is-hidden"
  let headerIcon = if isExpanded then Fa.Solid.AngleDown else Fa.Solid.AngleUp

  let title =
    if errors.Length = 0 then
      span [ ] [ str "Problems" ]
    else
      span [ ] [
        str "Problems: ";
        Text.span [ Props [ Style [ MarginLeft ".5rem" ] ] ] [
          str (string errors.Length )
        ]
      ]

  div [ Class "scrollable-panel is-problem" ] [
    div [ Class "scrollable-panel-header"
          OnClick (fun _ -> dispatch ToggleProblemsPanel) ] [
      div [ Class "scrollable-panel-header-icon" ] [
        Icon.icon [ ] [ Fa.i [ headerIcon; Fa.Size Fa.FaLarge ] [] ]
      ]
      div [ Class "scrollable-panel-header-title" ] [ title ]
      div [ Class "scrollable-panel-header-icon" ] [
        Icon.icon [ ] [
          Fa.i [ headerIcon; Fa.Size Fa.FaLarge ] []
        ]
      ]
    ]
    div [ Class ("scrollable-panel-body " + bodyDisplay) ] [
      for error in errors do
        match error.severity with
        | Monaco.Severity.Error
        | Monaco.Severity.Warning ->
          let (icon, colorClass) =
            match error.severity with
            | Monaco.Severity.Error -> Fa.Solid.TimesCircle, ofColor IsDanger
            | Monaco.Severity.Warning -> Fa.Solid.ExclamationTriangle, ofColor IsWarning
            | _ -> failwith "Should not happen", ofColor NoColor

          yield
            div [ Class ("scrollable-panel-body-row " + colorClass)
                  OnClick (fun _ ->
                    ReactEditor.Dispatch.cursorMove "sanuki_cursor_jump" error
                  ) ] [
              Icon.icon [ Icon.Size IsSmall
                          Icon.CustomClass colorClass ]
                        [ Fa.i [ icon ] [] ]
              span [ Class "scrollable-panel-body-row-description" ] [ str error.message ]
              span [ Class "scrollable-panel-body-row-position" ] [
                str "("
                str (string error.startLineNumber)
                str ","
                str (string error.startColumn)
                str ")"
              ]
            ]
        | _ -> ()
    ]
  ]


let private registerCompileCommand dispatch (editor : Monaco.Editor.IStandaloneCodeEditor) (monacoModule : Monaco.IExports) =
  let triggerCompile =
    { new Monaco.Editor.ICommandHandler with member __.Invoke _ = dispatch StartCompile }
  editor.addCommand(
    float (int monacoModule.KeyMod.Alt ||| int Monaco.KeyCode.Enter),
    triggerCompile,
    ""
  ) |> ignore
  editor.addCommand(
    float (int monacoModule.KeyMod.CtrlCmd ||| int Monaco.KeyCode.KEY_S),
    triggerCompile,
    ""
  ) |> ignore

let inline private numberToPercent number =
  string (number * 100.) + "%"

let private fontSizeClass =
  function
  | 11. -> "is-small"
  | 14. -> "is-medium"
  | 17. -> "is-large"
  | _ -> "is-medium"

[<Literal>]
let private DEFAULT_FONT_SIZE = 14.

[<Literal>]
let private MONACO_DEFAULT_FONT_FAMILY = "Menlo, Monaco, \"Courier New\", monospace"

let private editorArea model dispatch =
    div [ Class "vertical-panel"
          Style [ Width (numberToPercent model.PanelSplitRatio)
                  Position PositionOptions.Relative ] ]
        [ ReactEditor.editor [ ReactEditor.Options (editorOptions
                                                        DEFAULT_FONT_SIZE
                                                        MONACO_DEFAULT_FONT_FAMILY)
                               ReactEditor.Value model.Code
                               ReactEditor.Errors model.Errors
                               ReactEditor.EventId "sanuki_cursor_jump"
                               ReactEditor.CustomClass (fontSizeClass DEFAULT_FONT_SIZE)
                               ReactEditor.EditorDidMount (fun editor monacoModule ->
                                if not (isNull editor) then
                                    dispatch (SetEditor editor)
                                    registerCompileCommand dispatch editor monacoModule
                                else dispatch LoadFail
                               ) ]
          problemsPanel model.IsProblemsPanelExpanded model.Errors dispatch ]

let private viewCodeEditor (model: Model) =
  let fontFamily = MONACO_DEFAULT_FONT_FAMILY
  let options =
    jsOptions<Monaco.Editor.IEditorConstructionOptions>(fun o ->
      let minimapOptions = jsOptions<Monaco.Editor.IEditorMinimapOptions>(fun oMinimap ->
          oMinimap.enabled <- Some false
      )
      o.language <- Some "sanuki"
      o.fontSize <- Some DEFAULT_FONT_SIZE
      o.theme <- Some "vs-dark"
      o.minimap <- Some minimapOptions
      o.readOnly <- Some true
      o.fontFamily <- Some fontFamily
    )

  ReactEditor.editor [ ReactEditor.Options options
                       ReactEditor.Value model.Result
                       ReactEditor.CustomClass (fontSizeClass DEFAULT_FONT_SIZE) ]

let private outputArea model dispatch =
  let content =
      [ div [ Class "output-content"; Style [Height "100%"] ] [ 
          if model.State = Loading || model.State = Compiling then
            yield
              div [ Class "is-loading title has-text-centered"
                    Style [Height "100%"; FontSize "1.2em"]] []
          else
            yield viewCodeEditor model ] ]

  div [ Class "output-container"
        Style [ Width (numberToPercent (1. - model.PanelSplitRatio)) ] ]
      content

let view (model: Model) dispatch =
  React.Common.lazyView2
    (fun model dispatch ->
      div [ ] [
        div [ Class "page-content" ] [
          div [ Class "main-content" ] [
            editorArea model dispatch
            outputArea model dispatch ] ] ]
    )
    model
    dispatch

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "app-container"
|> Program.run
