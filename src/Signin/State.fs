module Signin.State
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
open Http

let (formState, formConfig) =
       FormBuilder.Form<Types.Msg>
            .Create(OnFormMsg)
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

            .Build()



let init _ =
    let (formState, formCmds) = Form.init formConfig formState
    EditingForm formState, Cmd.map OnFormMsg formCmds


let update (model:Model) msg =
    match msg with 
    |OnFormMsg msg -> 
        match model with 
        |EditingForm f -> 
            let (formState, formCmd) = Form.update formConfig msg f
            EditingForm formState, formCmd
    |Submit ->
        match model with 
        |EditingForm x ->
            let (newFormState,isValid) = Form.validate formConfig x
            let body = Form.toJson formConfig newFormState
            let newModel = x |> Form.setLoading true
            EditingForm newModel,
                Cmd.ofPromise (post "http://localhost:5000/signin" AuthUser.Decoder) body Response FailSignin
    
    |Response auth ->
        match model with 
        | EditingForm form -> 
            let newModel = form |> Form.setLoading false
            let status = Encode
            
    |FailSignin e -> model,[]
    
    |_ -> model,[]