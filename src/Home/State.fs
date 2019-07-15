
module Home.State
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
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open System
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Fable.PowerPack
open Thoth.Json
open Fable.Import

open Fulma
open Fable.Core.JsInterop
open Thoth.Json

let s = obj
type test = {
    name: string
    surname: string
 }
 let object (values: (string * obj) seq) =
        let o = obj()
        for (key, value) in values do
            o?key <- value
        box o

let createProfileFromServer (body: string) =
    promise {
         do! Promise.sleep 1000
         match Decode.fromString FormData.Decoder body with
         | Result.Ok data ->
             return if data.Name = "Test" then
                     Encode.toString 4 (Encode.object [
                                                       "code", Encode.string "errors"
                                                       "data", Encode.list
                                                                   [
                                                                     Encode.object
                                                                         [
                                                                             "Key", Encode.string "name"
                                                                             "Text", Encode.string "it's name already used"
                                                                         ]
                                                                   ]
                                                      ])
                    else
                     Encode.toString 4 (Encode.object [ "code", Encode.string "ok" ])
         | Error e -> return failwith e
    }
let createProfile (body: string): JS.Promise<CreationResponse> =
    printf "createProfile"

    promise {
         let! data = createProfileFromServer body
         printf "data %A" data
         let decoder =
            Decode.field "code" Decode.string
            |> Decode.andThen (
                function
                | "ok" ->
                    Decode.succeed CreationResponse.Ok

                | "errors" ->

                   Decode.succeed (CreationResponse.Errors [ { Key = "name"; Text = "text" } ])

                | unkown ->
                    sprintf "`%s` is an unkown code" unkown
                    |> Decode.fail
            )

         let result =
            match Decode.fromString decoder data with
            | Result.Ok result ->
                printf "result %A" result
                result
            | Error msg ->
                printf "error"
                failwith msg
         return result
    }


let getLanguages123() =
        promise {
            do! Promise.sleep 2000
            return [
                "1", "C"
                "10", "C#"
                "4", "Clojure"
                "7", "Elm"
                "9", "F#"
                "269", "JavaScript"
            ]
        }
let decodeIntFromString =
        Decode.string
        |> Decode.map int

let decoderAllowPublic: Decode.Decoder<bool> =
        decodeIntFromString
        |> Decode.andThen (
            function
            | 1 ->
                Decode.succeed true

            | 2 ->
                Decode.succeed false

            | unkown ->
                sprintf "`%i` isn't a valid representation for publicSetting" unkown
                |> Decode.fail
        )

let getDataFromServer =
        promise {
            do! Promise.sleep 1000
            return Encode.object [
                            "code", Encode.string "errors"
                        ]
        }

let getLanguages =
    promise {
        // We are calling directly `FakeServer.getLanguages` but in a
        // real case you would make a server request
        return! getLanguages123()
    }
let (formState, formConfig) =
       Thoth.Elmish.FormBuilder.Form<Thoth.Elmish.FormBuilder.Types.Msg>
        .Create(OnFormMsg)
        .AddField(
            BasicInput
                .Create("name")
                .WithLabel("Name")
                .IsRequired()
                .WithDefaultView())
        .AddField(
            BasicSelect
                .Create("favLanguage")
                .WithLabel("Favorite language")
                .WithValuesFromServer(getLanguages)
                .WithPlaceholder("")
                .IsRequired("I know it's hard but you need to choose")
                .WithDefaultView()
                )
        .AddField(
            BasicTextarea
                .Create("description")
                .WithLabel("Description")
                .IsRequired()
                .WithPlaceholder("Here you can introduce yourself...")
                .AddValidator(fun state ->
                    if state.Value.Length < 10 then
                        Types.Invalid "You need to enter a description of at least 10 characters"
                    else
                        Types.Valid
                )
                .WithDefaultView()
        )
        .AddField(
            BasicCheckbox
                .Create("condition")
                .WithLabel("I agree with the terms and conditions")
                .IsRequired()
                .WithDefaultView()
        )
        .AddField(
            BasicRadio
                .Create("publicSetting")
                .WithLabel("Make your profile public ?")
                .IsRequired()
                .WithValues([
                    "1", "Yes"
                    "2", "No"
                ])
                .WithDefaultView()
        )
        .Build()

let applyIfEditing model f =
    match model with
    | Editing m -> f m
    | Completed -> Completed, []

let private formActions (formState: FormBuilder.Types.State) dispatch =
    div []
        [ button [ OnClick(fun _ ->
                    dispatch Submit
                   ) ]
            [ str "Submit" ] ]


let viewFormEditing model dispatch =
    Form.render
        { Config = formConfig
          State = model.FormState
          Dispatch = dispatch
          ActionsArea = (formActions model.FormState dispatch)
          Loader = Form.DefaultLoader }


let view (model: Model) dispatch =
    let content =
        match model with
        | Editing formState ->
            div []
                [ Message.message [ Message.Color IsInfo ]
                    [ Message.body []
                        [ str "If you want to test the server side validation feature, enter \"Test\" in the \"Name\" field." ] ]
                  viewFormEditing formState dispatch ]
        | Completed ->
            Message.message [ Message.Color IsInfo ]
                [ Message.header []
                    [ str "Your profile has been created" ]
                  Message.body []
                    [ str "The demo will reset in a few seconds" ] ]

    div [ Style [ MaxWidth "500px"
                  MinHeight "530px"
                  PaddingTop "10px" ] ]
        [ content ]
let init _ =
    let (formState, formCmds) = Form.init formConfig formState
    Editing { FormState = formState; StringValue = "" }, Cmd.map OnFormMsg formCmds


type JsonValue = obj

type ErrorReason =
    | BadPrimitive of string * JsonValue
    | BadPrimitiveExtra of string * JsonValue * string
    | BadType of string * JsonValue
    | BadField of string * JsonValue
    | BadPath of string * JsonValue * string
    | TooSmallArray of string * JsonValue
    | FailMessage of string
    | BadOneOf of string list

type DecoderError = string * ErrorReason
type Decoder<'T> = string -> JsonValue -> Result<'T, DecoderError>

let update msg model: Model * Cmd<Home.Types.Msg> =
    let (>=>) f1 f2 x =
        let f3 x =
            match f1 x with
            | Some x -> f2 x
            | None -> None
        f3 x

    let s = Some >> (fun x -> printf "1"; x)
             >=> Some >> (fun x -> printf "2"; x)
               >=> Some >> (fun x -> printf "3"; x) >> ignore
    s 1
    let (>=>) (f1: Decoder<'a> -> Decoder<'a>) (f2: Decoder<'a> -> Decoder<'a>) : Decoder<'a> =
         fun a b ->
             match f1 a b with
             | Ok a 

    let q = Decode.int >=> Decode.int


    match msg with
    | SuccessResponse response ->
        applyIfEditing model
            (fun x ->
                printf "Success Response"
                match response with
                | Ok ->
                    let _ = x.FormState |> Form.setLoading false
                    Completed, []
                | Errors errors ->
                    let newFormState =
                                x.FormState
                                |> Form.setLoading false
                                |> Form.setErrors formConfig errors
                    printf "newFormState %A" newFormState
                    Editing { x with FormState = newFormState }, Cmd.none
             )


    | FailResponse ->
        applyIfEditing model
            (fun model ->
                printf "FailResponse"
                let state = model.FormState |> Form.setLoading false
                Editing { model with FormState = state }, []
            )

    | Submit ->
        applyIfEditing model
            (fun model ->
                printf "Submit"
                let (newFormState, isValid) = Form.validate formConfig model.FormState
                if isValid then
                    let body = Form.toJson formConfig newFormState
                    let newModel = model.FormState |> Form.setLoading true
                    Editing { model with FormState = newModel },
                     Cmd.ofPromise createProfile body SuccessResponse (fun x -> FailResponse)
                else
                    Editing model, []
             )

    | OnFormMsg msg ->
        applyIfEditing model
            (fun model ->
                let (formState, formCmd) = Form.update formConfig msg model.FormState
                printf "formState"
                Editing { model with FormState = formState }, Cmd.map OnFormMsg formCmd
            )
