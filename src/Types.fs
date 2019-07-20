module App.Types

open Global

type Msg =
    | CounterMsg of Counter.Types.Msg
    | SignupMsg of Signup.Types.Msg
    | SigninMsg of Signin.Types.Msg
    | HomeMsg of Home.Types.Msg
    | NavbarMsg of Navbar.Types.Msg

type AuthModel = {
  CurrentPage: Page
  Home:Home.Types.Model
  UserInfo : UserInfo.AuthUser
}

type UnAuthModel = 
  {
    CurrentPage: Page
    Signup: Signup.Types.Model
    Signin:Signin.Types.Model
  }

type Model =
   | UnAuth of UnAuthModel
   | Auth of AuthModel
