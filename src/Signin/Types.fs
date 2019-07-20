module Signin.Types
open Thoth.Elmish

type Msg = 
    | SuccessResponse of UserInfo.AuthUser
    | ErrorResponse of (string*(string list))
    | FailSignin of exn
    | Submit
    | OnFormMsg of FormBuilder.Types.Msg
    | ToAuth of UserInfo.AuthUser

type Model = 
    | Validating of (string list)*FormBuilder.Types.State
    | EditingForm of FormBuilder.Types.State
    | Completed of UserInfo.AuthUser