module App.Types

open Global

type Msg =
    | CounterMsg of Counter.Types.Msg
    | SignupMsg of Signup.Types.Msg

type Model =
    { CurrentPage: Page
      Counter: Counter.Types.Model
      Signup: Signup.Types.Model }
