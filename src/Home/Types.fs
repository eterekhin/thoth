module Home.Types
open System
open Thoth.Elmish
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Thoth.Json

let stringToInt = Decode.string |> Decode.map int

let decodeAllowPublic =
    stringToInt
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

type FormData = {
    Name:string;
    Email:string
    Password:string
    AllowEmail :bool;
    Specialty:int
}
 with static member Decoder =
        Decode.object (fun get ->
              {
               Name = get.Required.Field "name" Decode.string
               Email = get.Required.Field "email" Decode.string
               Specialty = get.Required.Field "specialty" Decode.int
               AllowEmail = get.Required.Field "allowEmail" decodeAllowPublic
               Password = get.Required.Field "password" Decode.string
              }
          )

type CreationResponse =
    |Ok
    |Errors of Types.ErrorDef list

type EditingModel =
    {
        FormState: FormBuilder.Types.State
        StringValue: string
    }

type Model =
    | Editing of EditingModel
    | Completed

type Msg =
    | ChangeStr of string
    | OnFormMsg of FormBuilder.Types.Msg
    | SuccessResponse of CreationResponse
    | FailResponse of Exception
    | Submit
