module Global

type Page =
    | Signup
    | Signin
    | Counter
    | About
    | Home 

let toHash page =
    match page with
    | Signup -> "#signup"
    | Signin -> "#signin"
    | Home -> "#home"
    | Counter -> "#counter"
