--module name must be same as filename and must start with capital letter
module Something exposing (Model) 

--Model the name of the model we've used in type alias
--import Html along with all its features
import Html exposing (..)
--To report back the events
import Html.Events exposing(..)
--To interact with Browser
import Browser

--Defining the model
-- model is just a data structure that contains important information about the application.
type alias Model = Int --our model's type is integer
initialModel : Model
--initially it is 0
initialModel = 0

--view is used to tell what is viewed to user
view : Model -> Html Msg
--this means that the Model will be converted to an HTML called msg

view model = div []
        [ button [ onClick Decrement ] [ text "-" ], -- this will throw Decrement when Clicked
          text (String.fromInt model),
          button [ onClick Increment ] [ text "+" ]  --this will throw Increment when Clicked
        ]

--this HTML msg has a div, that div has a button, text, button       
--each button has a text

--main is the starting point of the program
main : Program () Model Msg
--defining init,view and update
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

--Till now we have displayed everything, now let's add functionalities i.e., the update part
type Msg
    = Increment
    | Decrement

--the part that needs updating is "Msg" , it'll change Model to Model, 
--i.e., the datatype will be same just value will change
update : Msg -> Model -> Model
update msg model =
--if msg == Increment then model+1, else if msg == Decrement then model-1
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1