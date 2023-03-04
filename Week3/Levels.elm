--The program starts with the Module name, The Module name must start with capital
module MaeFol exposing(..)

--Importing the necessary modules
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
import Frame3d exposing(..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Decoder)
import Length exposing (Meters, millimeters)
import List exposing(concat)
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
--BUT WHAT IS AN ID EXACTLY ?? You can assign IDs to your entities and then tell add the IDs to functions
type Id
    = Mouse --To assign it to a pointer
    | Floor --To create a surface to place the Ball and Buildings
    | Ball --To assign to the ball
    | Building1 --To assign to the building, although I think we can replace Building2 by Building1
    | Obstacle (Block3d Meters BodyCoordinates)

--defining the model, the entirity.. 
type alias Model =
    { world : World Id 
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , maybeRaycastResult : Maybe (RaycastResult Id)
    , camera : Camera3d Meters WorldCoordinates
    }

--defining the possible output from the Model.. these msgs decide where the program goes next, i.e., update
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
init : () -> ( Model, Cmd Msg ) --it'll take nothing and return the Model and Cmd Msg
init _ =
    ( { world = initialWorld
      , width = pixels 0
      , height = pixels 0
      , maybeRaycastResult = Nothing
      , camera =
            Camera3d.perspective
                 { viewpoint =
                Viewpoint3d.lookAt
                { eyePoint = Point3d.millimeters 400 -472 10000
                , focalPoint = Point3d.millimeters 400 -472 300
                , upDirection = Direction3d.positiveZ
                }
                   , verticalFieldOfView = Angle.degrees 24
                }
      }
    , Task.perform
        (\{ viewport } ->
            Resize (round viewport.width) (round viewport.height)
        )
        Browser.Dom.getViewport
    )


--What you need to keep looking for 
--You analyze the model and return a Sub Msg
subscriptions : Model -> Sub Msg
subscriptions _ = --it returns Resize when Browser.Events.onResize is done, and similarly returns AnimationFrame
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
        |> World.add slope
        |> World.add (Body.plane Floor)

ne x y =
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+1200) (y-200) 400)
      ,
      Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+200) (y-1200) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x+500) y 0)
        (Point3d.millimeters (x+700) (y-500) 400)
    ]
--standard block of height 300 (100 to 400) and width 200
--1200,200 --x coordinate y coordinate x length y length
nel x y a b= 
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+a) (y-200) 400)
      ,
     Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+200) (y-b) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x+((a/2)-100)) y 0)
        (Point3d.millimeters (x+((a/2)+100)) (y-(b/2)) 400)
    ]
sw x y =
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-1200) (y+200) 400)
      ,
      Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-200) (y+1200) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x-500) y 0)
        (Point3d.millimeters (x-700) (y+500) 400)
    ]

swl x y a b= 
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-a) (y+200) 400)
      ,
     Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-200) (y+b) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x-((a/2)-100)) y 0)
        (Point3d.millimeters (x-((a/2)+100)) (y+(b/2)) 400)
    ]

nw x y =
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+1200) (y+200) 400)
      ,
      Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+200) (y+1200) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x+500) y 0)
        (Point3d.millimeters (x+700) (y+500) 400)
    ]

nwl x y a b= 
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+a) (y+200) 400)
      ,
     Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x+200) (y+b) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x+((a/2)-100)) y 0)
        (Point3d.millimeters (x+((a/2)+100)) (y+(b/2)) 400)
    ]
se x y =
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-1200) (y-200) 400)
      ,
      Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-200) (y-1200) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x-500) y 0)
        (Point3d.millimeters (x-700) (y-500) 400)
    ]
sel x y a b= 
    [ Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-a) (y-200) 400)
      ,
     Block3d.from
        (Point3d.millimeters x y 0)
        (Point3d.millimeters (x-200) (y-b) 400)
      ,
      Block3d.from
        (Point3d.millimeters (x-((a/2)-100)) y 0)
        (Point3d.millimeters (x-((a/2)+100)) (y-(b/2)) 400)
    ]

--Defining the ball with dimensions
ballBlock : List (Sphere3d.Sphere3d Meters BodyCoordinates)
ballBlock =
    [ Sphere3d.atPoint (Point3d.millimeters 0 -2500 100)
        (Length.millimeters 150)
    ]

--Defining the buiding with dimensions
buildingBlock1 : List (Block3d Meters BodyCoordinates)
buildingBlock1 = [
    Block3d.from
        (Point3d.millimeters 1000 2000 1000)
        (Point3d.millimeters -1000 500 1100),
    Block3d.from
        (Point3d.millimeters 1000 -2000 0)
        (Point3d.millimeters -1000 -3000 100),
        Block3d.from
        (Point3d.millimeters 1000 2000 1100)
        (Point3d.millimeters -1000 2100 4000)
    
    ]
--concat[(nel -1200 3600 1000 2500),(nel -200 3600 1000 1000), (sel 2000 3600 1200 2000 ), (sel 2000 1600 2500 2000), (nwl -1200 -4500 3200 4000), (nwl -1200 -1200 2000 4000 ), (sel 2000 0 1500 4300 ), (sel 2000 300 2000 5000)]
--concat[(nel -1200 3600 1800 1200),(nwl 340 1990 1000 1200), (swl 2200 -4000 2000 3400 ), (sel 632 707 17000 1388)]



    -- , Block3d.from
    --     (Point3d.millimeters -275 -275 400)
    --     (Point3d.millimeters 275 275 450)
    --]

--Something to do with IDs

slope : Body Id
slope =
    let
        slopeBlock = 
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.millimeters 2000
                , Length.millimeters 2000
                , Length.millimeters 100
                )

    in
    
        Body.block slopeBlock (Obstacle slopeBlock)
            |> Body.rotateAround Axis3d.x (Angle.degrees 35)
            -- |> Body.rotateAround Axis3d.y (Angle.degrees 120)
            |> Body.moveTo (Point3d.millimeters 10 -2000 0)


ball : Body Id --
ball =
    Body.compound (List.map Physics.Shape.sphere ballBlock) Ball
        |> Body.withBehavior (Body.dynamic (kilograms 3.58))

building1 : Body Id
building1 =
    Body.compound (List.map Physics.Shape.block buildingBlock1) Building1
        |> Body.withBehavior (Body.static)

-- building2 : Body Id
-- building2 =
--     Body.compound (List.map Physics.Shape.block buildingBlock2) Building2
--         |> Body.withBehavior (Body.dynamic (kilograms 3.58))

--camera settings, read Camera3d Docs..
camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.millimeters 0 200 10000
                , focalPoint = Point3d.millimeters 0 200 1300
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
                Scene3d.sphere (Material.matte Color.red)
                    (Sphere3d.atOrigin (millimeters 20))

            Ball ->
                ballBlock
                    |> List.map
                        (Scene3d.sphereWithShadow
                            (Material.nonmetal
                                { baseColor = Color.white
                                , roughness = 0.25
                                }
                            )
                        )
                    |> Scene3d.group

            Obstacle hehe ->
                Scene3d.blockWithShadow
                (Material.nonmetal
                  { baseColor = Color.blue
                  , roughness = 1
                  }
                )
              hehe
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
            -- Building2 ->
            --     buildingBlock2
            --         |> List.map
            --             (Scene3d.blockWithShadow
            --                 (Material.nonmetal
            --                     { baseColor = Color.green
            --                     , roughness = 0.25
            --                     }
            --                 )
            --             )
            --         |> Scene3d.group
            Floor ->
                Scene3d.quad (Material.matte Color.darkCharcoal)
                    (Point3d.meters -15 -15 0)
                    (Point3d.meters -15 15 0)
                    (Point3d.meters 15 15 0)
                    (Point3d.meters 15 -15 0)


update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimationFrame -> --if msg equals AnimationFrame Return the model 
            { model | world = World.simulate (seconds (1 / 60)) model.world }
            

        Resize width height -> --if msg equals Resize
            { model --Return the model with following with width and height resized
                | width = Pixels.float (toFloat width)
                , height = Pixels.float (toFloat height)
            }

        MouseDown mouseRay -> --if msg equals MouseDown, 
            case World.raycast mouseRay model.world of --
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
                                , camera =
                                         Camera3d.perspective
                                             { viewpoint =
                                                 Viewpoint3d.lookAt
                                                     { eyePoint = worldPoint
                                                     , focalPoint = Point3d.millimeters 0 0 0
                                                     , upDirection = Direction3d.positiveZ
                                                     }
                                             , verticalFieldOfView = Angle.degrees 24
                                            }
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
                            , camera =
                                         Camera3d.perspective
                                             { viewpoint =
                                                 Viewpoint3d.lookAt
                                                     { eyePoint = worldPoint
                                                     , focalPoint = Point3d.millimeters 400 -472 300
                                                     , upDirection = Direction3d.positiveZ
                                                     }
                                             , verticalFieldOfView = Angle.degrees 24
                                            }
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