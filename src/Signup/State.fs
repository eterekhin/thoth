module Signup.State
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Elmish
open Types
open Fable.PowerPack
open Thoth.Json
open Fable.Import
open Fable.Core
open Fable.Import.React
open UserInfo
open Fable.PowerPack.Fetch.Fetch_types
open Http

let createProfileFromServer (body: string) =
    promise {

        let defaultProps =
              [
                RequestProperties.Method HttpMethod.POST;
                Fetch.requestHeaders [ ContentType "application/json" ]
                RequestProperties.Body(body |> unbox) ]

        let! response = Fetch.postRecord "http://localhost:8080/signup" body defaultProps

        let text = response.text();
        return! text;
    }


let createProfile (body: string): JS.Promise<CreationResponse> =
    printf "createProfile"
    promise {
         let! data = createProfileFromServer body
         printf "createResponse data:%A" data
         let decoder =
            Decode.field "code" Decode.string
            |> Decode.andThen (
                function
                | "ok" ->
                   Decode.field "data" (AuthUser.Decoder |> Decode.map  CreationResponse.Ok)

                | "error" ->
                   let decodeTuple = Decode.tuple2 Decode.string (Decode.string |> Decode.list)
                   Decode.field "data"
                           (decodeTuple)
                            |> Decode.map (fun xx ->
                               let (name,errs) = xx;
                               CreationResponse.Errors (errs |> List.map (fun x -> { Key = name; Text = x })))

                | unkown ->
                    sprintf "`%s` is an unkown code" unkown
                    |> Decode.fail
            )

         let result =
            match Decode.fromString decoder data with
            | Result.Ok result ->
                result
            | Error msg ->
                failwith msg
         return result
    }


let getSpecialties() =
        promise {
            do! Promise.sleep 2000
            return [
                "1", "Developer"
                "2", "Tester"
                "3", "Analytic"
                "4", "Team-Lead"
                "5", "Architecter"
                "6", "Manager"
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

[<Emit("null")>]
let emptyElement : ReactElement= jsNative

let getLanguages =
    promise {
        // We are calling directly `FakeServer.getLanguages` but in a
        // real case you would make a server request
        return! getSpecialties()
    }
let (formState, formConfig) =
       FormBuilder.Form<Types.Msg>
            .Create(OnFormMsg)
            .AddField(
                BasicInput
                    .Create("name")
                    .WithLabel("Name")
                    .IsRequired()
                    .WithDefaultView())
            .AddField(
                BasicInput
                    .Create("email")
                    .WithLabel("Email")
                    .IsRequired()
                    .AddValidator(fun x ->
                        if System.Text.RegularExpressions.Regex.IsMatch(x.Value, @"^\S+@\S+\.\S+$")
                        then Valid
                        else Invalid "Email is not valid")
                    .WithDefaultView())

            .AddField(
                BasicInput
                    .Create("password")
                    .WithLabel("Pasword")
                    .IsRequired()
                    .AddValidator(fun x ->
                        if x.Value.Length < 5
                        then Invalid "Password must be longer than 5 characters"
                        else Types.Valid)
                    .WithDefaultView())

            .AddField(
                BasicSelect
                    .Create("specialty")
                    .WithLabel("Specialty")
                    .WithValuesFromServer(getLanguages)
                    .WithPlaceholder("")
                    .IsRequired("I know it's hard but you need to choose")
                    .WithDefaultView())

            .AddField(
                BasicCheckbox
                    .Create("allowEmail")
                    .WithLabel("  I agree with the terms and conditions")
                    .IsRequired()
                    .WithDefaultView())
            .Build()

let applyIfEditing model f =
    match model with
    | Editing m -> f m
    | Completed x-> Completed x, []


let init _ =
    let (formState, formCmds) = Form.init formConfig formState
    Editing { FormState = formState; StringValue = "" }, Cmd.map OnFormMsg formCmds



let update msg model: Model * Cmd<Signup.Types.Msg> =
    match msg with
    | SuccessResponse response ->
        applyIfEditing model
            (fun x ->
                match response with
                | Ok signup->
                    let _ = x.FormState |> Form.setLoading false
                    printf "%A" signup
                    Completed signup, []
                | Errors errors ->
                    let newFormState =
                                x.FormState
                                |> Form.setLoading false
                                |> Form.setErrors formConfig errors
                    Editing { x with FormState = newFormState }, Cmd.none
             )

    | FailResponse exn ->
        applyIfEditing model
            (fun model ->
                let state = model.FormState |> Form.setLoading false
                Editing { model with FormState = state }, []
            )

    | Submit ->
        applyIfEditing model
            (fun model ->
                let (newFormState, isValid) = Form.validate formConfig model.FormState
                if isValid then
                    let body = Form.toJson formConfig newFormState
                    let newModel = model.FormState |> Form.setLoading true
                    Editing { model with FormState = newModel },
                     Cmd.ofPromise createProfile body SuccessResponse FailResponse
                else
                    Editing model, []
             )

    | OnFormMsg msg ->
        applyIfEditing model
            (fun model ->
                let (formState, formCmd) = Form.update formConfig msg model.FormState
                Editing { model with FormState = formState }, Cmd.map OnFormMsg formCmd
            )
