module Global
open Fable.Core
open Fable.Core.Exceptions
open Fable.Import

open Fable.Core.JsInterop
[<Emit("undefined")>]
let undefined : obj = jsNative

[<Emit("process.env")>]
let env :obj = jsNative
let host = 
     if env?PORT = undefined 
     then @"http://localhost/"
     else @"https://note-subscribe-api/"

let port = 
    if env?PORT = undefined 
    then 4200 
    else int env?PORT

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
