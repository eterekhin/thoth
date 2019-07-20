module Signup.View
open Types
open Fulma
open Fable.Helpers.React
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Signup.State



let private formActions (formState: FormBuilder.Types.State) dispatch =
    div []
        [ button [ OnClick(fun _ ->
                    dispatch Submit
                   ) ]
            [ str "Signup" ] ]



let viewFormEditing model dispatch =
    Form.render
        { Config = formConfig
          State = model
          Dispatch = dispatch
          ActionsArea = (formActions model dispatch)
          Loader = Form.DefaultLoader }


let root (model: Model) dispatch =
    let content =
        match model with
        | Editing formState ->
            div []
                [ Message.message [ Message.Color IsInfo ]
                    [ Message.body []
                        [ str "Hello, signup please" ] ]
                  viewFormEditing formState dispatch ]
        | Validating(errors,formState) ->
            div []
                [ Message.message [ Message.Color IsInfo ]
                    [ Message.body []
                        [ str "Hello, signup please" ] ]
                  viewFormEditing formState dispatch 
                  div[Style[MarginTop "20px"]][]
                  Message.message [ Message.Color IsDanger ]
                      [ Message.body []
                         (errors |> List.map str) ]
                ]
            
    div [ Style [ MaxWidth "500px"
                  MinHeight "530px"
                  PaddingTop "10px" ] ]
        [ content ]
