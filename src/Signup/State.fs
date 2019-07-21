module Signup.State
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Elmish
open Types
open Fable.PowerPack
open Thoth.Json
open Fable.Import
open UserInfo
open Fable.PowerPack.Fetch.Fetch_types

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
    promise {
         let! data = createProfileFromServer body
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




let init _ =
    let (formState, formCmds) = Form.init formConfig formState
    Editing formState, Cmd.map OnFormMsg formCmds



let update msg model: Model * Cmd<Signup.Types.Msg> =
    match model with
    |Editing formState ->    
        match msg with
        | SuccessResponse response ->
            match response with
            | Ok userInfo->
                let _ = formState |> Form.setLoading false
                printf "%A" userInfo
                Editing formState, ToAuth userInfo |> Cmd.ofMsg
            | Errors errors ->
                let newFormState =
                            formState
                            |> Form.setLoading false
                Validating (errors |> List.map(fun x -> x.Text),newFormState), Cmd.none

        | FailResponse exn ->
            let state = formState |> Form.setLoading false
            Editing formState, []

        | Submit ->
            let (newFormState, isValid) = Form.validate formConfig formState
            if isValid then
                let body = Form.toJson formConfig newFormState
                let newModel = formState |> Form.setLoading true
                Editing formState,
                 Cmd.ofPromise createProfile body SuccessResponse FailResponse
            else
                Editing formState, []

        | OnFormMsg msg ->
            let (formState, formCmd) = Form.update formConfig msg formState
            Editing formState, Cmd.map OnFormMsg formCmd
        | ToAuth(_) -> failwith "Not Implemented"
    |Validating (_,formState) -> Editing formState,Cmd.none 
