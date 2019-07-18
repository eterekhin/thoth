module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open UserInfo
 
let pageParser: Parser<Page->Page,Page> =
    oneOf [
        map About (s "about")
        map Counter (s "counter")
        map Signup (s "signup")
    ]

let urlUpdate (result : Page option) model =
    match result with
    | None ->
        console.error("Error parsing url")
        model, Navigation.modifyUrl (toHash model.CurrentPage)
    | Some page ->
        { model with CurrentPage = page }, []

let init result =
    let (counter, counterCmd) = Counter.State.init()
    let (signup, signupCmd) = Signup.State.init()
    let (model, cmd) =
        urlUpdate result
          { CurrentPage = Signup
            Counter = counter
            Signup = signup
            User = User.UnAuthUser }

    model, Cmd.batch [ cmd
                       Cmd.map CounterMsg counterCmd
                       Cmd.map SignupMsg signupCmd ]

let update msg model =
    match msg with
    | CounterMsg msg ->
        let (counter, counterCmd) = Counter.State.update msg model.Counter
        { model with Counter = counter }, Cmd.map CounterMsg counterCmd
    | SignupMsg msg ->
        let (signup, signupCmd) = Signup.State.update msg model.Signup
        { model with Signup = signup }, Cmd.map SignupMsg signupCmd
