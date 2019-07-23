module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open UserInfo
open Http
open Fable.Import
open Fable.Core.JsInterop
open Fable.Import.Browser
open Fable.Core.Exceptions

let pageParser: Parser<Page->Page,Page> =
    oneOf [
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

let unAuthInit() = 
    let (signup, signupCmd) = Signup.State.init()
    let (signin,signinCmd) = Signin.State.init();
    let model = UnAuth {CurrentPage = Signin;Signup = signup;Signin = signin}
    model, Cmd.batch([
                            Cmd.map SignupMsg signupCmd;
                            Cmd.map SigninMsg signinCmd])

let authInit user =
    let (home,homeCmd) = Home.State.init()
    Auth {CurrentPage=Home;UserInfo = user;Home = home},homeCmd

let toAuthRedirect user = 
    let (model,cmd) = authInit user;
    (model,Cmd.batch 
                [
                        (Navigation.modifyUrl "#home" ) ;
                         cmd
                ])

let toUnAuthRedirect =
    let (model,cmd) = unAuthInit()
    (model,Cmd.batch[
                    Navigation.modifyUrl "#signin";
                     cmd 
                    ])

let init result =
    let tryGetJwt = localStorage.getItem "jwt"
    if tryGetJwt = undefined  then 
        let (model,cmd) = unAuthInit()
        let (newModel,newcmd) = urlUpdate (Some Signin) model
        newModel,cmd@newcmd
    else 
         let jwt = tryGetJwt |> string
         let (model,_) = unAuthInit()
         let (UnAuth unAuth) = model; 
         UnAuth{unAuth with CurrentPage = Loading},
            Cmd.ofPromise (getWithAuthorize "getUserInfo" AuthUser.Decoder) jwt OnSuccessUserInfoLoaded OnFailedUserInfoLoaded
         


let update msg (model:Model) =
    match model with
   
    | UnAuth unAuth ->
        match msg with
        | OnSuccessUserInfoLoaded httpResult ->
            match httpResult with
             | Correct authUserInfo ->    toAuthRedirect authUserInfo
             
             | Failed _ -> toUnAuthRedirect
        
        | OnFailedUserInfoLoaded s -> model,[]
       
        | SignupMsg msg ->
            match msg with 
            | Signup.Types.Msg.ToAuth user ->  
                localStorage.setItem ("jwt",user.Token)
                toAuthRedirect user

            | _ ->  let (signup, signupCmd) = Signup.State.update msg unAuth.Signup
                    UnAuth{ unAuth with Signup = signup }, Cmd.map SignupMsg signupCmd
        
        | SigninMsg msg -> 
             match msg with 
             | Signin.Types.Msg.ToAuth user ->  
                localStorage.setItem ("jwt",user.Token)
                toAuthRedirect user
          
             | _ ->   
                let (signinModel,signinCmd) = Signin.State.update unAuth.Signin msg
                UnAuth {unAuth with Signin = signinModel}, Cmd.map SigninMsg signinCmd
        | HomeMsg(_) -> failwith "Not Implemented"
        | NavbarMsg(_) -> failwith "Not Implemented"
        | OnFailedUserInfoLoaded(_) -> failwith "Not Implemented"
    
    |Auth auth -> 
        match msg with
        |NavbarMsg msg ->
            match msg with 

            |Logout -> 
                localStorage.removeItem "jwt"
                toUnAuthRedirect
            
        |HomeMsg msg ->
            let (home, homeCmd) = Home.State.update msg auth.Home
            Auth { auth with Home = home }, Cmd.map HomeMsg homeCmd
        | SignupMsg(_) -> failwith "Not Implemented"
        | SigninMsg(_) -> failwith "Not Implemented"
        | OnSuccessUserInfoLoaded(_) -> failwith "Not Implemented"
        | OnFailedUserInfoLoaded(_) -> failwith "Not Implemented"
       