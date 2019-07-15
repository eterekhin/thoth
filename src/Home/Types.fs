module Home.Types
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
    FavLanguage:string;
    Description:string;
    Condition :bool;
    AllowPublic:bool
}
 with static member Decoder =
        Decode.object (fun get ->
              {
               Name = get.Required.Field "name" Decode.string
               FavLanguage = get.Required.Field "favLanguage" Decode.string
               Description = get.Required.Field "description" Decode.string
               Condition = get.Required.Field "condition" Decode.bool
               AllowPublic = get.Required.Field "publicSetting" decodeAllowPublic
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
    | FailResponse
    | Submit
