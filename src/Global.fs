module Global

type Page =
    | Signup
    | Signin
    | Counter
    | About

let toHash page =
    match page with
    | Signup -> "#signup"
    | Signin -> "#signin"
    | About -> "#about"
    | Counter -> "#counter"
