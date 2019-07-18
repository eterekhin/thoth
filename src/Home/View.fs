module Home.View
open Types
open Fulma
open Fable.Helpers.React
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Home.State
let private formActions (formState: FormBuilder.Types.State) dispatch =
    div []
        [ button [ OnClick(fun _ ->
                    dispatch Submit
                   ) ]
            [ str "Signup" ] ]

let root (model: Model) dispatch =
    let content =
        match model with
        | Editing formState ->
            div []
                [ Message.message [ Message.Color IsInfo ]
                    [ Message.body []
                        [ str "Hello, signup please" ] ]
                  viewFormEditing formState dispatch ]
        | Completed ->
            Message.message [ Message.Color IsInfo ]
                [ Message.header []
                    [ str "You have successfully registered" ]
                  Message.body []
                    [ str "Redirect to homepage ..." ] ]

    div [ Style [ MaxWidth "500px"
                  MinHeight "530px"
                  PaddingTop "10px" ] ]
        [ content ]
