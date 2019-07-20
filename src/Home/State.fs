module Home.State
open Home.Types
open Elmish
let init() =  Model,[]

let update msg model : Model*Cmd<Msg> = 
    model,Cmd.none