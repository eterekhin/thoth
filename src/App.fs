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
open Fable.Import.React


//#region Menus
let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]

let menuBuilder items currentPage = 
 aside
      [ ClassName "menu" ]
      [ p
          [ ClassName "menu-label" ]
          [ str "General" ]
        ul
          [ ClassName "menu-list" ]
           (items |> List.map(fun f -> f currentPage))
        ]

let root model dispatch = 
  let pageHtml page =
    match model with 
    | UnAuth unAuthModel ->
            match page with
            | Page.About -> Info.View.root
            | Signup -> Signup.View.root unAuthModel.Signup (SignupMsg >> dispatch)
            | Signin -> Signin.View.root unAuthModel.Signin (SigninMsg >> dispatch)
    
    | Auth authModel ->
      match page with
      | Home -> Home.View.root authModel.Home (HomeMsg >> dispatch)

  let unAuthItems = [menuItem "Signup" Signup 
                     menuItem "Signin" Signin]

  let authItems = [menuItem "Home" Home];
  let view menuItems currentPage = 
    div
      []
      [ Navbar.View.root (NavbarMsg >> dispatch)
        div
          [ ClassName "section" ]
          [ div
              [ ClassName "container" ]
              [ div
                  [ ClassName "columns" ]
                  [ 
                    menuBuilder menuItems currentPage
                    div
                      [ ClassName "column" ]
                      [ pageHtml currentPage ] 
                      
                   ] ]] ]
  match model with 
  | Auth authModel -> view authItems authModel.CurrentPage
  | UnAuth unAuthModel -> view unAuthItems unAuthModel.CurrentPage

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
