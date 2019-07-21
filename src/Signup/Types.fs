module Signup.Types
open System
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Thoth.Json

type CreationResponse =
    |Ok of UserInfo.AuthUser
    |Errors of Types.ErrorDef list


type Model =
    | Validating of (string list)*FormBuilder.Types.State
    | Editing of FormBuilder.Types.State

type Msg =
    | OnFormMsg of FormBuilder.Types.Msg
    | SuccessResponse of CreationResponse
    | FailResponse of Exception
    | Submit
    | ToAuth of UserInfo.AuthUser
