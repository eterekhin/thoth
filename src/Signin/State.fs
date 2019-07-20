module Signin.State
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Elmish
open Types
open UserInfo
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
        |EditingForm f | Validating(_,f) -> 
            printf "OnFormMsg"
            let (formState, formCmd) = Form.update formConfig msg f
            EditingForm formState,  Cmd.map OnFormMsg formCmd

    |Submit ->
        printf "Submit"
        match model with 
        |EditingForm x ->
            let (newFormState,isValid) = Form.validate formConfig x
            match isValid with 
            |true ->
                let body = Form.toJson formConfig newFormState
                let newModel = x |> Form.setLoading true
                EditingForm newModel,
                    (Cmd.ofPromise (post "http://localhost:8080/signin" AuthUser.Decoder failDecoder)
                        body 
                        (function 
                         | Correct s-> SuccessResponse s 
                         | Failed f -> ErrorResponse f
                        )
                        FailSignin)
            |false -> model,[]
    
    |SuccessResponse auth ->
        printf "successResponse %A" auth
        match model with 
        | EditingForm form -> 
            let newModel = form |> Form.setLoading false
            EditingForm newModel, ToAuth auth |> Cmd.ofMsg 

    | ErrorResponse errorMsg -> 
        printf "ErrorResponse"
        match model with 
        | EditingForm form ->
            let (_,errorText) = errorMsg
            let newModel = form  |> Form.setLoading false
            Validating (errorText,newModel),[]
            
    |FailSignin _ -> 
        printf "failed"
        model,[]
    