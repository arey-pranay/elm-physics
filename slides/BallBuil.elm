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
import Viewpoint3d

--defining what all can be the IDs, or the return values
--BUT WHAT IS AN ID EXACTLY ??
type Id
    = Mouse
    | Floor
    | Ball
    | Building1
    | Building2

--defining the model, the entirity
type alias Model =
    { world : World Id
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , maybeRaycastResult : Maybe (RaycastResult Id)
    }

--defining the possible input-output
type Msg
    = AnimationFrame
    | Resize Int Int
    | MouseDown (Axis3d Meters WorldCoordinates)
    | MouseMove (Axis3d Meters WorldCoordinates)
    | MouseUp

--defining what will be displayed first
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }

--inital state of all components
init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld
      , width = pixels 0
      , height = pixels 0
      , maybeRaycastResult = Nothing
      }
    , Task.perform
        (\{ viewport } ->
            Resize (round viewport.width) (round viewport.height)
        )
        Browser.Dom.getViewport
    )


--What you need to keep looking for
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
        ]

--How the world would look initially
initialWorld : World Id
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
        |> World.add ball
        |> World.add building1
        |> World.add building2
        |> World.add (Body.plane Floor)

--Defining the ball with dimensions
ballBlock : List (Sphere3d.Sphere3d Meters BodyCoordinates)
ballBlock =
    [ Sphere3d.atPoint (Point3d.millimeters 400 -472 300)
        (Length.millimeters 200)
    ]

--Defining the buiding with dimensions
buildingBlock1 : List (Block3d Meters BodyCoordinates)
buildingBlock1 =
    [ Block3d.from
        (Point3d.millimeters 1 -200 100)
        (Point3d.millimeters -200 200 800)
    ]

buildingBlock2 : List (Block3d Meters BodyCoordinates)
buildingBlock2 =
    [ Block3d.from
        (Point3d.millimeters 1200 -600 100)
        (Point3d.millimeters 1400 -1000 800)
    ]
    
    -- , Block3d.from
    --     (Point3d.millimeters -272 -272 0)
    --     (Point3d.millimeters -222 -222 400)
    -- , Block3d.from
    --     (Point3d.millimeters 222 -272 0)
    --     (Point3d.millimeters 272 -222 400)
    -- , Block3d.from
    --     (Point3d.millimeters -275 -275 400)
    --     (Point3d.millimeters 275 275 450)
    --]

--Something to do with IDs
ball : Body Id
ball =
    Body.compound (List.map Physics.Shape.sphere ballBlock) Ball
        |> Body.withBehavior (Body.dynamic (kilograms 3.58))

building1 : Body Id
building1 =
    Body.compound (List.map Physics.Shape.block buildingBlock1) Building1
        |> Body.withBehavior (Body.dynamic (kilograms 3.58))

building2 : Body Id
building2 =
    Body.compound (List.map Physics.Shape.block buildingBlock2) Building2
        |> Body.withBehavior (Body.dynamic (kilograms 3.58))

--camera settings, read Camera3d Docs..
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

--What will be viewed (HTML) 
view : Model -> Html Msg
view { world, width, height } =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "mousedown" (decodeMouseRay camera width height MouseDown)
        , Html.Events.on "mousemove" (decodeMouseRay camera width height MouseMove)
        , Html.Events.onMouseUp MouseUp
        ]
        --Setting the scene up. Scene3d Docs
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
            Mouse ->
                Scene3d.sphere (Material.matte Color.white)
                    (Sphere3d.atOrigin (millimeters 20))

            Ball ->
                ballBlock
                    |> List.map
                        (Scene3d.sphereWithShadow
                            (Material.nonmetal
                                { baseColor = Color.blue
                                , roughness = 0.25
                                }
                            )
                        )
                    |> Scene3d.group
            Building1 ->
                buildingBlock1
                    |> List.map
                        (Scene3d.blockWithShadow
                            (Material.nonmetal
                                { baseColor = Color.green
                                , roughness = 0.25
                                }
                            )
                        )
                    |> Scene3d.group
            Building2 ->
                buildingBlock2
                    |> List.map
                        (Scene3d.blockWithShadow
                            (Material.nonmetal
                                { baseColor = Color.green
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimationFrame ->
            { model | world = World.simulate (seconds (1 / 60)) model.world }

        Resize width height ->
            { model
                | width = Pixels.float (toFloat width)
                , height = Pixels.float (toFloat height)
            }

        MouseDown mouseRay ->
            case World.raycast mouseRay model.world of
                Just raycastResult ->
                    case Body.data raycastResult.body of
                        Ball ->
                            let
                                worldPoint =
                                    Point3d.placeIn
                                        (Body.frame raycastResult.body)
                                        raycastResult.point

                                mouse =
                                    Body.compound [] Mouse
                                        |> Body.moveTo worldPoint
                            in
                            { model
                                | maybeRaycastResult = Just raycastResult
                                , world =
                                    model.world
                                        |> World.add mouse
                                        |> World.constrain
                                            (\b1 b2 ->
                                                case ( Body.data b1, Body.data b2 ) of
                                                    ( Mouse, Ball ) ->
                                                        [ Constraint.pointToPoint
                                                            Point3d.origin
                                                            raycastResult.point
                                                        ]

                                                    _ ->
                                                        []
                                            )
                            }

                        _ ->
                            model
                      
                        -- Building ->
                        --     let
                        --         worldPoint =
                        --             Point3d.placeIn
                        --                 (Body.frame raycastResult.body)
                        --                 raycastResult.point

                        --         mouse =
                        --             Body.compound [] Mouse
                        --                 |> Body.moveTo worldPoint
                        --     in
                        --     { model
                        --         | maybeRaycastResult = Just raycastResult
                        --         , world =
                        --             model.world
                        --                 |> World.add mouse
                        --                 |> World.constrain
                        --                     (\b1 b2 ->
                        --                         case ( Body.data b1, Body.data b2 ) of
                        --                             ( Mouse, Building ) ->
                        --                                 [ Constraint.pointToPoint
                        --                                     Point3d.origin
                        --                                     raycastResult.point
                        --                                 ]

                        --                             _ ->
                        --                                 []
                        --                     )
                        --     }

                        -- _ ->
                        --     model

                Nothing ->
                    model

        MouseMove mouseRay ->
            case model.maybeRaycastResult of
                Just raycastResult ->
                    let
                        worldPoint =
                            Point3d.placeIn
                                (Body.frame raycastResult.body)
                                raycastResult.point

                        plane =
                            Plane3d.through
                                worldPoint
                                (Viewpoint3d.viewDirection (Camera3d.viewpoint camera))
                    in
                    { model
                        | world =
                            World.update
                                (\body ->
                                    if Body.data body == Mouse then
                                        case Axis3d.intersectionWithPlane plane mouseRay of
                                            Just intersection ->
                                                Body.moveTo intersection body

                                            Nothing ->
                                                body

                                    else
                                        body
                                )
                                model.world
                    }

                Nothing ->
                    model

        MouseUp ->
            { model
                | maybeRaycastResult = Nothing
                , world =
                    World.keepIf
                        (\body -> Body.data body /= Mouse)
                        model.world
            }


decodeMouseRay :
    Camera3d Meters WorldCoordinates
    -> Quantity Float Pixels
    -> Quantity Float Pixels
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay camera3d width height rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg
                (Camera3d.ray
                    camera3d
                    (Rectangle2d.with
                        { x1 = pixels 0
                        , y1 = height
                        , x2 = width
                        , y2 = pixels 0
                        }
                    )
                    (Point2d.pixels x y)
                )
        )
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)