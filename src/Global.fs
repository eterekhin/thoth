module Global

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
