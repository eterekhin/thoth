module Global
open Fable.Core

[<Emit("undefined")>]
let undefined : obj = jsNative

[<Emit("production")>]
let production:bool= jsNative
let host = 
     if not production 
     then @"http://localhost:8080"
     else @"https://note-subscribe-api.herokuapp.com/"

type Page =
    | Signup
    | Signin
    | Home 
    | Loading

let toHash page =
    match page with
    | Signup -> "#signup"
    | Signin -> "#signin"
    | Home -> "#home"
