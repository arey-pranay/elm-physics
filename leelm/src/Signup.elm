module Signup exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)

--Let's define our model again
type alias User =
    { name : String
    , email : String
    , password : String
    , loggedIn : Bool
    }

--Let's define what the model (user) will initially have

initialModel : User --Initial State of User
initialModel =
    { name = ""
    , email = ""
    , password = ""
    , loggedIn = False
    }

--Let's tell what will be viewed to user as the HTML page

view : User -> Html msg 
--One division, with One Heading and Four Sub-Divisions, will be visible
view user =
    div []
        [ h1 [] [ text "Sign up" ]
        , Html.form []
            [ div []
                [ text "Name"
                , input [ id "name", type_ "text" ] []
                ]
            , div []
                [ text "Email"
                , input [ id "email", type_ "email" ] []
                ]
            , div []
                [ text "Password"
                , input [ id "password", type_ "password" ] []
                ]
            , div []
                [ button [ type_ "submit" ]
                    [ text "Create my account" ]
                ]
            ]
        ]

--Here comes the -"Application Entry Point"
main : Html msg
main =
    view initialModel
