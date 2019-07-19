module Signup.Types
open System
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Thoth.Json





type CreationResponse =
    |Ok of UserInfo.AuthUser
    |Errors of Types.ErrorDef list

type EditingModel =
    {
        FormState: FormBuilder.Types.State
        StringValue: string
    }

type Model =
    | Editing of EditingModel
    | Completed of UserInfo.AuthUser

type Msg =
    | ChangeStr of string
    | OnFormMsg of FormBuilder.Types.Msg
    | SuccessResponse of CreationResponse
    | FailResponse of Exception
    | Submit
