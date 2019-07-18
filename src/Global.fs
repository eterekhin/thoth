module Global

type Page =
    | Signup
    | Signin
    | Home
    | Counter
    | About

let toHash page =
    match page with
    | Signup -> "#signup"
    | Signin -> "#signin"
    | About -> "#about"
    | Counter -> "#counter"
    | Home -> "#home"
