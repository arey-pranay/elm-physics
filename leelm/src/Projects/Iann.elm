module Iann exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Sphere3d exposing (Sphere3d)
import Task
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)

type SphereCoordinates
    = SphereCoordinates

sphere : Sphere3d Meters SphereCoordinates
sphere =
    Sphere3d.withRadius (Length.centimeters 5) Point3d.origin

main : Html msg
main =
    Scene3d.unlit
        { entities =
            [ Scene3d.sphere(Material.color Color.blue)
            ]
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 5 2 3
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        }