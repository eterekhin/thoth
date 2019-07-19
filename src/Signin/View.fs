module Signin.View
open Fulma
open Fable.Helpers.React
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Signin.Types
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Elmish
open Types
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack
open Thoth.Json
open Fable.Import
open Fable.Core
open Fable.Import.React
open UserInfo
open Fable.PowerPack.Fetch.Fetch_types
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
    match model with 
    |EditingForm formState -> 
            let content = 
                div []
                    [ Message.message [ Message.Color IsInfo ]
                        [ Message.body []
                            [ str "Hello, signin please" ] ]
                      viewFormEditing formState dispatch ]
            div [ Style [ MaxWidth "500px"
                          MinHeight "530px"
                          PaddingTop "10px" ] ]
                [content]

    |_ -> div [] []