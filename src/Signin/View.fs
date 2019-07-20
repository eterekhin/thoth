module Signin.View
open Fulma
open Fable.Helpers.React
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Fable.Helpers.React.Props
open Signin.Types
open Signin.State

let private formActions (formState: FormBuilder.Types.State) dispatch =
    div []
        [ button [ OnClick(fun _ ->
                    dispatch Submit
                   ) ]
            [ str "Signin" ] ]


let viewFormEditing (formState:Types.State) dispatch =
    Form.render
        { Config = formConfig
          State = formState
          Dispatch = dispatch
          ActionsArea = (formActions formState dispatch)
          Loader = Form.DefaultLoader }


let root model dispatch = 
    let content =match model with 
                |EditingForm formState -> 
                                div []
                                    [ Message.message [ Message.Color IsInfo ]
                                        [ Message.body []
                                            [ str "Hello, signin please" ] ]
                                      viewFormEditing formState dispatch
                                    ]
                |Validating (errors,formState) ->
                                  div []
                                    [ Message.message [ Message.Color IsInfo ]
                                        [ Message.body []
                                            [ str "Hello, signin please" ] ]
                                      viewFormEditing formState dispatch
                                      div
                                       [Style[MarginTop "20px"]]
                                       [Message.message [ Message.Color IsDanger ]
                                         [ Message.body []
                                        (errors |> List.map(str))]]]

                |_ -> div [] []

    div [ Style [ MaxWidth "500px"
                  MinHeight "530px"
                  PaddingTop "10px" ] ]
                [content]