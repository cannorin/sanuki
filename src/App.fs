module App.View

open Elmish
open Fable.React
open Fable.React.Props
open Fable.FontAwesome
open Fulma


type Model =
    { Value : string; Result: string }

type Msg =
    | ChangeValue of string

let init _ = { Value = ""; Result = "" }, Cmd.none

let private update msg model =
    match msg with
    | ChangeValue newValue ->
      { model with Value = newValue; Result = Sanuki.test newValue |> sprintf "%A" }, Cmd.none

let private view model dispatch =
    Hero.hero [ Hero.IsFullHeight ]
        [ Hero.body [ ]
            [ Container.container [ ]
                [ Columns.columns [ Columns.CustomClass "has-text-centered" ]
                    [ Column.column [ Column.Width(Screen.All, Column.IsOneThird)
                                      Column.Offset(Screen.All, Column.IsOneThird) ]
                        [ Image.image [ Image.Is128x128
                                        Image.Props [ Style [ Margin "auto"] ] ]
                            [ img [ Src "assets/fulma_logo.svg" ] ]
                          Field.div [ ]
                            [ Label.label [ ]
                                [ str "Enter code" ]
                              Control.div [ ]
                                [ Textarea.textarea [ Textarea.OnChange (fun s -> ChangeValue s.Value |> dispatch) ]
                                    [ ] ] ]
                          Content.content [ ]
                            [ 
                              str model.Result
                            ] ] ] ] ] ]

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
