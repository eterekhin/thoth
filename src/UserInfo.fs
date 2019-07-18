module UserInfo
open Thoth.Json
type Token = |Token of string
//SignupUserDto
type AuthUser = {
    Username : string
    Token :string
    PrimarySpecialties:string list
}
//
 with static member Decoder: Decode.Decoder<AuthUser> =
            Decode.object(
                    fun get ->
                    {
                        Username = get.Required.Field "username" Decode.string
                        Token = get.Required.Field "token" Decode.string
                        PrimarySpecialties = get.Required.Field "primarySpecialties"  (Decode.list Decode.string)
                    })



type User = 
|UnAuthUser 
|AuthUser of AuthUser