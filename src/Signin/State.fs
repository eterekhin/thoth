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
        | Completed(_) -> failwith "Not Implemented"

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
                    (Cmd.ofPromise (post "signin" AuthUser.Decoder)
                        body 
                        (fun f -> match f with | Correct s -> SuccessResponse s | Failed (errorKey,errorText)-> ErrorResponse (errorKey,errorText))
                        FailSignin)
            |false -> model,[]
        | Validating(_, _) -> failwith "Not Implemented"
        | Completed(_) -> failwith "Not Implemented"
    
    |SuccessResponse auth ->
        printf "successResponse %A" auth
        match model with 
        | EditingForm form -> 
            let newModel = form |> Form.setLoading false
            EditingForm newModel, ToAuth auth |> Cmd.ofMsg
        | Validating(_, _) -> failwith "Not Implemented"
        | Completed(_) -> failwith "Not Implemented" 

    | ErrorResponse (_,errorText) -> 
        printf "ErrorResponse"
        match model with 
        | EditingForm form ->
            let newModel = form  |> Form.setLoading false
            Validating (errorText,newModel),[]
        | Validating(_, _) -> failwith "Not Implemented"
        | Completed(_) -> failwith "Not Implemented"
            
    |FailSignin _ -> 
        printf "failed"
        model,[]
    