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

--Arrange the code acc to the type of code pieces
--Remove all the code related to mouse, and render just a sphere, then add keboard event listeners
--Observe how keyboard elements are decoded

type Id
    = Mouse --The mouse pointer is something
    | Floor --The Floor is something
    | Ball --The Ball is something


type alias Model = -- Model is being defined as an alias for a record type (a type that contains multiple named fields). The colon (:) is used to separate the field name from its type. 
    { world : World Id --World is the type of the field, and Id is a type parameter that specifies what kind of IDs the world uses.
    , width : Quantity Float Pixels --width is a field, Quantity is the type and Pixels is what it takes
    , height : Quantity Float Pixels
    , maybeRaycastResult : Maybe (RaycastResult Id)
    }--The Maybe type constructor is used here to indicate that the field may or may not contain a RaycastResult value. 


type Msg
    = AnimationFrame
    | Resize Int Int
    | MouseDown (Axis3d Meters WorldCoordinates)
    | MouseMove (Axis3d Meters WorldCoordinates)
    | MouseUp
-- AnimationFrame: This message is sent to the update function each time the browser requests a new animation frame, usually around 60 times per second.
-- Resize Int Int: This message is sent when the window size is changed, and it contains two Int values representing the new width and height of the window.
-- MouseDown (Axis3d Meters WorldCoordinates): This message is sent when the mouse button is pressed, and it contains an Axis3d Meters WorldCoordinates value that represents the position of the mouse click in 3D space.
-- MouseMove (Axis3d Meters WorldCoordinates): This message is sent when the mouse is moved, and it contains an Axis3d Meters WorldCoordinates value that represents the new position of the mouse in 3D space.
-- MouseUp: This message is sent when the mouse button is released.

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }
-- init: a function that takes no arguments and returns an initial state of the model.
-- update: a function that takes a message and the current model and returns a new model and a command (a command is an instruction that can be used to perform some side effect, like sending an HTTP request or triggering a timeout).
-- subscriptions: a function that specifies which messages the application should listen to and how to convert them to the Msg type.
-- view: a function that takes the current model and returns an HTML representation of the user interface.


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

-- init is a function that takes a single argument of type () (unit), which is typically used when there is no meaningful value to pass in. 
-- The function returns a tuple consisting of the initial model and a command that should be executed.
-- In this case, the initial model is created as a record with fields world, width, height, and maybeRaycastResult, where 
-- world is initialized to the result of initialWorld function call, and the other fields are initialized to 0 and Nothing respectively.
-- The command that is returned by init is a Task.perform that will asynchronously call the Browser.Dom.getViewport function 
-- to obtain the current size of the viewport. The result of this task is wrapped in a Resize message constructor and 
-- sent to the update function, where it will be used to set the width and height fields of the model.


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
        ]

-- the subscriptions function takes in a Model value as input, but since this program does not need to listen to any changes in the model, it ignores the argument by using an underscore _ as a placeholder variable.
-- The function returns a Sub.batch value, which is a helper function for combining multiple subscriptions into a single Sub Msg value. In this case, the Sub.batch function combines two subscriptions:
-- Browser.Events.onResize Resize: This subscription will trigger a Resize message whenever the browser window is resized. The Resize message will contain the new width and height of the browser window in pixels.
-- Browser.Events.onAnimationFrame (\_ -> AnimationFrame): This subscription will trigger an AnimationFrame message on each animation frame (usually about 60 times per second). The _ in (\_ -> AnimationFrame) indicates that we don't need the argument passed to the function, and we just want to return an AnimationFrame message.


initialWorld : World Id
initialWorld =
    World.empty
        |> World.withGravity (Acceleration.gees 1) Direction3d.negativeZ
        |> World.add ball
        |> World.add (Body.plane Floor)
--initialWorld variable, which is a value of type World Id. The initialWorld value is created using the World.empty
--Here, the World.withGravity function is used to add gravity to the world. The Acceleration.gees function is used to define the strength of the gravitational force in gees
--The World.add function is then used twice to add the ball and Body.plane Floor objects to the world.


circleList : List (Sphere3d.Sphere3d Meters BodyCoordinates) --i.e., a Sphere3d object can handle meters and bodycoordinates
circleList =
    [ Sphere3d.atPoint (Point3d.millimeters -222 272 400)
        (Length.millimeters 200) --try adding more elements to this list by looking at the table
    ]
--circleList is a list containing a single element of type Sphere3d.Sphere3d Meters BodyCoordinates
--The Meters type and BodyCoordinates type are type parameters to Sphere3d.Sphere3d.
--Meters is a type defined in the Length module, and represents a length in meters.
--BodyCoordinates is a type defined in the CoordinateSystem module, and represents coordinates relative to the center of mass of a bod


ball : Body Id
ball =
    Body.compound (List.map Physics.Shape.sphere circleList) Ball
        |> Body.withBehavior (Body.dynamic (kilograms 3.58))
--This code defines a ball as a Body using the compound function from the Body module to create a compound shape. Can be used in making maze


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
--The perspective projection is created using the Camera3d.perspective function, which takes a record of configuration options. These options include a Viewpoint3d describing the position and orientation of the camera, as well as a verticalFieldOfView parameter which determines the extent of the viewable area in the vertical direction.
--In this code, the viewpoint parameter of the Camera3d.perspective function is set to the result of calling the Viewpoint3d.lookAt function. This function takes three arguments: the eyePoint, which is the position of the camera; the focalPoint, which is the point in the scene that the camera is looking at; and the upDirection, which is the direction that is considered "up" in the scene.



view : Model -> Html Msg
view { world, width, height } =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "mousedown" (decodeMouseRay camera width height MouseDown)
        , Html.Events.on "mousemove" (decodeMouseRay camera width height MouseMove)
        , Html.Events.onMouseUp MouseUp
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
--The view is a div that has the following attributes: "position": "absolute" "left": "0" "top": "0" These make sure the div is positioned at the top left corner of the screen. 
--The div has the following event listeners attached to it: "mousedown": This triggers the decodeMouseRay function with the camera, width, height and MouseDown event as inputs. "mousemove": This triggers the decodeMouseRay function with the camera, width, height and MouseMove event as inputs. "mouseup": This triggers the MouseUp event.
--Inside the div, there is a 3D scene rendered using the Scene3d.sunny function. The scene has the following properties. upDirection is set to Direction3d.z. sunlightDirection is set to a specific direction using the Direction3d.xyZ function. shadows is set to True. camera is set to the camera variable defined earlier. dimensions is set to the width and height of the view. background is set to Scene3d.transparentBackground. clipDepth is set to Length.meters 0.1. entities are the entities to be rendered in the scene, which are created by mapping bodyToEntity over the bodies in the world.


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
--The bodyToEntity function takes a Body value and returns an Entity value that can be rendered in the 3D scene.
-- First, it extracts the data field of the Body value, which is an identifier indicating the type of object represented by the body.
-- Then, it creates a Scene3d.placeIn function that takes the frame of the body as an argument, and places the rendered object in the scene at that position and orientation.
-- For a Mouse object, it creates a white sphere of 20mm radius using Scene3d.sphere function.
-- For a Ball object, it creates a compound shape of multiple spheres using the circleList value defined elsewhere in the code, and wraps the group of spheres in a single Scene3d.group entity.
-- For a Floor object, it creates a dark quad plane of 30 meters width and length using Scene3d.quad function. Finally, it returns the resulting entity.


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
--This code defines the update function, which takes a Msg and a Model and returns a new Model that incorporates the changes requested by the message.
--If the message is MouseDown, the function checks if there is a raycast intersection with the ball, and if so, creates a new "mouse" body at the intersection point and adds it to the world. The function also sets the maybeRaycastResult field of the model to the raycast result. If there is no intersection with the ball, the function does nothing.
--If the message is MouseMove and there is a maybeRaycastResult, the function moves the "mouse" body to the intersection point between the mouse ray and the plane that contains the intersection point and is perpendicular to the view direction of the camera. The function updates the world field of the model with the new position of the "mouse" body.
--If the message is MouseUp, the function removes the "mouse" body from the world and sets the maybeRaycastResult field of the model to Nothing.



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
--This function is a JSON decoder that decodes the mouse position on the screen into a 3D ray in world coordinates. The decoded value is a function that takes an Axis3d Meters WorldCoordinates argument (a 3D ray in world coordinates) and returns a message of some type msg. This message will be used to update the model when the mouse is clicked or moved.