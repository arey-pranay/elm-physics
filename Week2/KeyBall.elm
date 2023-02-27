module TryCircle exposing(..)
import Acceleration
import Angle
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Circle3d exposing (Circle3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Duration exposing (seconds)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Length exposing (Meters, millimeters)
import Mass exposing (kilograms)
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape
import Physics.World as World exposing (RaycastResult, World)
import Pixels exposing (Pixels, pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d exposing (Entity)
import Scene3d.Material as Material
import Sphere3d
import Task
import Html.Events exposing (on)
import Json.Decode as Json
import Keyboard exposing (..)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Viewpoint3d

type Id
    = Floor
    | Ball

type alias Model =
    { world : World Id
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , maybeRaycastResult : Maybe (RaycastResult Id)
    , sphereState : { position : BodyCoordinates, velocity : Axis3d Meters WorldCoordinates }
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld
      , width = pixels 0
      , height = pixels 0
      , maybeRaycastResult = Nothing
      , sphereState = { position = 0 0 0, velocity = Axis3d.z }
      }
    , Task.perform
        (\{ viewport } ->
            Resize (round viewport.width) (round viewport.height)
        )
        Browser.Dom.getViewport
    )

type Msg
    = AnimationFrame
    | Resize Int Int
    | KeyDown 
    | KeyUp 

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
        ]

initialWorld : World Id
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
        |> World.add ball
        |> World.add (Body.plane Floor)


circleList : List (Sphere3d.Sphere3d Meters BodyCoordinates) --i.e., a Sphere3d object can handle meters and bodycoordinates
circleList =
    [ Sphere3d.atPoint (Point3d.millimeters -222 272 400)
        (Length.millimeters 300) --try adding more elements to this list by looking at the table
    ]


ball : Body Id
ball =
    Body.compound (List.map Physics.Shape.sphere circleList) Ball
        |> Body.withBehavior (Body.dynamic (kilograms 3.58))

camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 3 4 2
                , focalPoint = Point3d.meters -0.5 -0.5 0
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 24
        }



view : Model -> Html Msg
view { world, width, height } =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "keydown" (decodeKeyboardEvent camera width height KeyDown)
        , Html.Events.on  KeyUp
        --Add HTML event listener for keyboard events
        ]
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
            , shadows = True
            , camera = camera
            , dimensions =
                ( Pixels.int (round (Pixels.toFloat width))
                , Pixels.int (round (Pixels.toFloat height))
                )
            , background = Scene3d.transparentBackground
            , clipDepth = Length.meters 0.1
            , entities = List.map bodyToEntity (World.bodies world)
            }
        ]

bodyToEntity : Body Id -> Entity WorldCoordinates
bodyToEntity body =
    let
        frame =
            Body.frame body

        id =
            Body.data body
    in
    Scene3d.placeIn frame <|
        case id of
            Ball ->
                circleList
                    |> List.map
                        (Scene3d.sphereWithShadow
                            (Material.nonmetal
                                { baseColor = Color.white
                                , roughness = 0.25
                                }
                            )
                        )
                    |> Scene3d.group

            Floor ->
                Scene3d.quad (Material.matte Color.darkCharcoal)
                    (Point3d.meters -15 -15 0)
                    (Point3d.meters -15 15 0)
                    (Point3d.meters 15 15 0)
                    (Point3d.meters 15 -15 0)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            let
                direction =
                    case key of
                        Keyboard.ArrowUp ->
                            vec3 0 0 -1

                        Keyboard.ArrowDown ->
                            vec3 0 0 1

                        Keyboard.ArrowLeft ->
                            vec3 -1 0 0

                        Keyboard.ArrowRight ->
                            vec3 1 0 0

                        _ ->
                            vec3 0 0 0
            in
            ( { model | velocity = model.velocity + direction }
            , Cmd.none
            )

        KeyDown ->
            let
                direction =
                    case key of
                        Keyboard.ArrowUp ->
                            vec3 0 0 1

                        Keyboard.ArrowDown ->
                            vec3 0 0 -1

                        Keyboard.ArrowLeft ->
                            vec3 1 0 0

                        Keyboard.ArrowRight ->
                            vec3 -1 0 0

                        _ ->
                            vec3 0 0 0
            in
            ( { model | velocity = model.velocity + direction }
            , Cmd.none
            )


-- update : Msg -> Model -> Model
-- update msg model =
--     case msg of
--         -- Handle existing messages as before
--         AnimationFrame ->
--             let
--                 newWorld =
--                     World.step Physics.timeStep model.world
--             in
--             { model | world = newWorld }

--         Resize newWidth newHeight ->
--             { model | width = pixels newWidth, height = pixels newHeight }

--         MouseDown _ ->
--             -- We don't need to do anything with mouse input for now
--             model

--         MouseMove _ ->
--             -- We don't need to do anything with mouse input for now
--             model

--         MouseUp ->
--             -- We don't need to do anything with mouse input for now
--             model

--         -- Handle new messages to control the sphere with keyboard input
--         KeyDown Browser.Key_Left ->
--             { model | sphereState = { model.sphereState | velocity = Axis3d.xyZ (Length.meters -1) (model.sphereState.velocity.z) } }

--         KeyDown Browser.Key_Right ->
--             { model | sphereState = { model.sphereState | velocity = Axis3d.xyZ (Length.meters 1) (model.sphereState.velocity.z) } }

--         KeyDown Browser.Key_Up ->
--             { model | sphereState = { model.sphereState | velocity = Axis3d.xyZ (model.sphereState.velocity.x) (Length.meters 1) } }

--         KeyDown Browser.Key_Down ->
--             { model | sphereState = { model.sphereState | velocity = Axis3d.xyZ (model.sphereState.velocity.x) (Length.meters -1) } }

--         KeyUp _ ->
--             -- We don't need to do anything when a key is released
--             model

-- module Main exposing (..)

-- import Html exposing (Html)
-- import Html.Events exposing (onKeyDown, onKeyUp)
-- import Json.Decode as Json
-- import Keyboard
-- import Mouse
-- import Transform exposing (..)
-- import Vector3 exposing (..)
-- import Color exposing (..)
-- import Material exposing (..)
-- import Scene exposing (..)
-- import Mesh exposing (..)
-- import MeshShapes exposing (..)


-- type alias Model =
--     { position : Vector3
--     , velocity : Vector3
--     , mesh : Mesh
--     }


-- init : ( Model, Cmd Msg )
-- init =
--     let
--         sphereMesh =
--             sphereMesh { radius = 1.0 } |> withMaterial (material { color = blue })
--     in
--     ( { position = vec3 0 0 0
--       , velocity = vec3 0 0 0
--       , mesh = sphereMesh
--       }
--     , Cmd.none
--     )


-- type Msg
--     = KeyPressed Keyboard.Key
--     | KeyReleased Keyboard.Key




-- view : Model -> Html Msg
-- view model =
--     let
--         transform =
--             rotate (Mouse.position * 0.01) (vec3 0 1 0)
--                 |> move model.position
--     in
--     toHtml (scene [ transform ]
--         [ model.mesh
--         ]
--     )


-- subscriptions : Model -> Sub Msg
-- subscriptions _ =
--     Keyboard.downs KeyPressed
--         |> Json.map KeyPressed
--         |> Sub.mapError (always (KeyReleased Keyboard.Shift)) -- prevent error on shift press
--         |> Sub.batch
--         |> Sub.mapError (Json.map KeyReleased)


-- main : Program () Model Msg
-- main =
--     Html.program
--         { init = init
--         , update = update
--         , view = view
--         , subscriptions = subscriptions
--         }
