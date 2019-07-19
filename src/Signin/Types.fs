
module Signin.Types
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

type Msg = 
    | Response of Result<UserInfo.AuthUser,ErrorDef list>
    | FailSignin of exn
    | Submit
    | OnFormMsg of FormBuilder.Types.Msg

type Model = 
    | EditingForm of FormBuilder.Types.State
    | Completed of UserInfo.AuthUser