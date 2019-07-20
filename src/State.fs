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
        map Signin (s "signin")
        map Home (s "home")
    ]


let urlUpdate (result : Page option) model =
    match model with 
    |Auth authModel ->
        match result with
        | None ->
            console.error("Error parsing url")
            model, Navigation.modifyUrl (toHash authModel.CurrentPage)
        | Some page ->
            Auth { authModel with CurrentPage = page }, []
    |UnAuth unAuth ->
          match result with
             | None ->
                console.error("Error parsing url")
                model, Navigation.modifyUrl (toHash unAuth.CurrentPage)
              | Some page ->
                UnAuth { unAuth with CurrentPage = page }, []


let init result =
    let (counter, counterCmd) = Counter.State.init()
    let (signup, signupCmd) = Signup.State.init()
    let (signin,signinCmd) = Signin.State.init();
    let (home,homeCmd) = Home.State.init()
    let (model, cmd) =
        urlUpdate result
                (UnAuth {CurrentPage = Signup
                         Signup = signup
                         Signin = signin
                        })

    model, Cmd.batch [ cmd
                       Cmd.map CounterMsg counterCmd
                       Cmd.map SignupMsg signupCmd ]

let update msg (model:Model) =
    match model with

    | UnAuth unAuth ->
        match msg with
        | SignupMsg msg ->
                    let (signup, signupCmd) = Signup.State.update msg unAuth.Signup
                    UnAuth{ unAuth with Signup = signup }, Cmd.map SignupMsg signupCmd
        | SigninMsg msg -> 
             match msg with 
                | Signin.Types.Msg.ToAuth user ->
                    let (home,_) = Home.State.init()
                    Auth {CurrentPage=Home;UserInfo = user;Home = home}, Navigation.modifyUrl "#home"
                | _ ->   
                    let (signinModel,signinCmd) = Signin.State.update unAuth.Signin msg
                    UnAuth {unAuth with Signin = signinModel}, Cmd.map SigninMsg signinCmd
    
    |Auth auth -> 
        match msg with
        |HomeMsg msg ->
            let (home, homeCmd) = Home.State.update msg auth.Home
            Auth { auth with Home = home }, Cmd.map HomeMsg homeCmd
