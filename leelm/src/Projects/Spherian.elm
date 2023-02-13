module Spherian exposing(..)
import Sphere3d exposing
    ( Sphere3d
    , atPoint, withRadius, atOrigin, throughPoints
    , centerPoint, radius, diameter, volume, surfaceArea, circumference, boundingBox
    , contains
    , scaleAbout, rotateAround, translateBy, translateIn, mirrorAcross, projectOnto, projectInto
    , at, at_
    , relativeTo, placeIn
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle2d exposing (Circle2d)
import Circle3d exposing (Circle3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types exposing (Sphere3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Cubed, Quantity, Rate, Squared)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)
type alias Sphere3d units coordinates =
    Types.Sphere3d units coordinates

atPoint : Point3d units coordinates -> Quantity Float units -> Sphere3d units coordinates
atPoint givenCenterPoint givenRadius =
    withRadius givenRadius givenCenterPoint

withRadius : Quantity Float units -> Point3d units coordinates -> Sphere3d units coordinates
withRadius givenRadius givenCenterPoint =
    Types.Sphere3d
        { radius = Quantity.abs givenRadius
        , centerPoint = givenCenterPoint
        }

atOrigin : Quantity Float units -> Sphere3d units coordinates
atOrigin givenRadius =
    atPoint Point3d.origin givenRadius

throughPoints : Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Point3d units coordinates -> Maybe (Sphere3d units coordinates)
throughPoints p1 p2 p3 p4 =
    Circle3d.throughPoints p1 p2 p3
        |> Maybe.andThen
            (\circle ->
                let
                    normalAxis =
                        Circle3d.axis circle

                    r =
                        Circle3d.radius circle

                    x =
                        Point3d.distanceFromAxis normalAxis p4

                    y =
                        Point3d.signedDistanceAlong normalAxis p4
                in
                if y /= Quantity.zero then
                    let
                        d =
                            (Quantity.squared r
                                |> Quantity.minus (Quantity.squared x)
                                |> Quantity.minus (Quantity.squared y)
                            )
                                |> Quantity.over
                                    (Quantity.multiplyBy -2 y)

                        computedRadius =
                            Quantity.sqrt
                                (Quantity.squared r
                                    |> Quantity.plus
                                        (Quantity.squared d)
                                )
                    in
                    Just <|
                        withRadius computedRadius (Point3d.along normalAxis d)

                else
                    Nothing
            )
-- at : Quantity Float (Rate units2 units1) -> Sphere3d units1 coordinates -> Sphere3d units2 coordinates
-- at rate (Types.Sphere3d sphere) =
--     Types.Sphere3d
--         { centerPoint = Point3d.at rate sphere.centerPoint
--         , radius = Quantity.abs (Quantity.at rate sphere.radius)
--         }

-- at_ : Quantity Float (Rate units1 units2) -> Sphere3d units1 coordinates -> Sphere3d units2 coordinates
-- at_ rate sphere =
--     at (Quantity.inverse rate) sphere


-- {-| Get the center point of a sphere.
-- -}
-- centerPoint : Sphere3d units coordinates -> Point3d units coordinates
-- centerPoint (Types.Sphere3d properties) =
--     properties.centerPoint


-- {-| Get the radius of a sphere.
-- -}
-- radius : Sphere3d units coordinates -> Quantity Float units
-- radius (Types.Sphere3d properties) =
--     properties.radius


-- {-| Get the diameter of a sphere (twice the radius).
-- -}
-- diameter : Sphere3d units coordinates -> Quantity Float units
-- diameter sphere =
--     Quantity.multiplyBy 2 (radius sphere)


-- {-| Get the circumference of a sphere (the circumference of a [great circle](https://en.wikipedia.org/wiki/Great_circle)
-- of the sphere).
-- -}
-- circumference : Sphere3d units coordinates -> Quantity Float units
-- circumference sphere =
--     Quantity.multiplyBy (2 * pi) (radius sphere)


-- {-| Get the surface area of a sphere.
-- -}
-- surfaceArea : Sphere3d units coordinates -> Quantity Float (Squared units)
-- surfaceArea sphere =
--     Quantity.multiplyBy (4 * pi) (Quantity.squared (radius sphere))


-- {-| Get the volume of a sphere.
-- -}
-- volume : Sphere3d units coordinates -> Quantity Float (Cubed units)
-- volume sphere =
--     Quantity.multiplyBy (4 / 3 * pi) (Quantity.cubed (radius sphere))


-- {-| Scale a sphere around a given point by a given scale.
-- -}
-- scaleAbout : Point3d units coordinates -> Float -> Sphere3d units coordinates -> Sphere3d units coordinates
-- scaleAbout point scale sphere =
--     withRadius (Quantity.multiplyBy (abs scale) (radius sphere))
--         (Point3d.scaleAbout point scale (centerPoint sphere))


-- {-| Rotate a sphere around a given axis by a given angle.
-- -}
-- rotateAround : Axis3d units coordinates -> Angle -> Sphere3d units coordinates -> Sphere3d units coordinates
-- rotateAround axis angle sphere =
--     withRadius (radius sphere)
--         (Point3d.rotateAround axis angle (centerPoint sphere))


-- {-| Translate a sphere by a given displacement.
-- -}
-- translateBy : Vector3d units coordinates -> Sphere3d units coordinates -> Sphere3d units coordinates
-- translateBy displacement sphere =
--     withRadius (radius sphere)
--         (Point3d.translateBy displacement (centerPoint sphere))


-- {-| Translate a sphere in a given direction by a given distance.
-- -}
-- translateIn : Direction3d coordinates -> Quantity Float units -> Sphere3d units coordinates -> Sphere3d units coordinates
-- translateIn direction distance sphere =
--     translateBy (Vector3d.withLength distance direction) sphere


-- {-| Mirror a sphere across a given plane.
-- -}
-- mirrorAcross : Plane3d units coordinates -> Sphere3d units coordinates -> Sphere3d units coordinates
-- mirrorAcross plane sphere =
--     withRadius (radius sphere)
--         (Point3d.mirrorAcross plane (centerPoint sphere))


-- {-| Take a sphere defined in global coordinates, and return it expressed in
-- local coordinates relative to a given reference frame.
-- -}
-- relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Sphere3d units globalCoordinates -> Sphere3d units localCoordinates
-- relativeTo frame sphere =
--     withRadius (radius sphere)
--         (Point3d.relativeTo frame (centerPoint sphere))


-- {-| Take a sphere considered to be defined in local coordinates relative to a
-- given reference frame, and return that sphere expressed in global coordinates.
-- -}
-- placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Sphere3d units localCoordinates -> Sphere3d units globalCoordinates
-- placeIn frame sphere =
--     withRadius (radius sphere)
--         (Point3d.placeIn frame (centerPoint sphere))


{-| Get the minimal bounding box containing a given sphere.

    Sphere3d.boundingBox exampleSphere
    --> BoundingBox3d.from
    -->     (Point3d.meters -2 -1 -2)
    -->     (Point3d.meters 4 5 4)

-}
boundingBox : Sphere3d units coordinates -> BoundingBox3d units coordinates
boundingBox sphere =
    let
        r =
            radius sphere

        p0 =
            centerPoint sphere

        cx =
            Point3d.xCoordinate p0

        cy =
            Point3d.yCoordinate p0

        cz =
            Point3d.zCoordinate p0
    in
    BoundingBox3d.fromExtrema
        { minX = cx |> Quantity.minus r
        , maxX = cx |> Quantity.plus r
        , minY = cy |> Quantity.minus r
        , maxY = cy |> Quantity.plus r
        , minZ = cz |> Quantity.minus r
        , maxZ = cz |> Quantity.plus r
        }


{-| Check if a sphere contains a given point.
-}
-- contains : Point3d units coordinates -> Sphere3d units coordinates -> Bool
-- contains point sphere =
--     point |> Point3d.distanceFrom (centerPoint sphere) |> Quantity.lessThanOrEqualTo (radius sphere)
--    Sphere3d.projectOnto Plane3d.xy exampleSphere
    --> Circle3d.withRadius (Length.meters 3)
    -->     Direction3d.z
    -->     (Point3d.meters 1 2 0)
projectOnto : Plane3d units coordinates -> Sphere3d units coordinates -> Circle3d units coordinates
projectOnto plane sphere =
    Circle3d.withRadius (radius sphere)
        (Plane3d.normalDirection plane)
        (Point3d.projectOnto plane (centerPoint sphere))


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a sphere into a sketch plane.

    Sphere3d.projectInto SketchPlane3d.xy exampleSphere
    --> Circle2d.withRadius (Length.meters 3)
    -->     (Point2d.meters 1 2)

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Sphere3d units coordinates3d -> Circle2d units coordinates2d
projectInto sketchPlane sphere =
    Circle2d.withRadius (radius sphere)
        (Point3d.projectInto sketchPlane (centerPoint sphere))