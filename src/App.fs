module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Types
open App.State
open Global
open Signin
importAll "../sass/main.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props

//#region Menus
let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]

let unAuthMenu currentPage= 
  aside
      [ ClassName "menu" ]
      [ p
          [ ClassName "menu-label" ]
          [ str "General" ]
        ul
          [ ClassName "menu-list" ]
          [ menuItem "Signup" Signup currentPage
            menuItem "Signin" Signin currentPage
       ] ]

let authMenu currentPage =
  aside
    [ ClassName "menu" ]
    [ p
        [ ClassName "menu-label" ]
        [ str "General" ]
      ul
        [ ClassName "menu-list" ]
        [ menuItem "Counter sample" Counter currentPage
          menuItem "About" Page.About currentPage ] ]
//#endregion

let root model dispatch =
  let pageHtml page =
    match page with
    | Page.About -> Info.View.root
    | Counter -> Counter.View.root model.Counter (CounterMsg >> dispatch)
    | Signup -> Signup.View.root model.Signup (SignupMsg >> dispatch)
    | Signin -> Signin.View.root model.Signin (SigninMsg >> dispatch)

  div
    []
    [ Navbar.View.root
      div
        [ ClassName "section" ]
        [ div
            [ ClassName "container" ]
            [ div
                [ ClassName "columns" ]
                [ 
                  (match model.Signup with 
                    |Signup.Types.Model.Completed s -> authMenu model.CurrentPage
                    |_ -> unAuthMenu  model.CurrentPage)
                  div
                    [ ClassName "column" ]
                    [ pageHtml model.CurrentPage ] 
                    
                 ] ]] ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
