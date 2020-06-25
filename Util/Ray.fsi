namespace Util

module Ray =
    type Vector = Vector.Vector
    type Point = Point.Point

    [<Sealed>]
    type Ray 
    val mkRay : o:Point -> d:Vector -> Ray
    val getOrigin : Ray -> Point
    val getDirection : Ray -> Vector
    val getComponents : Ray -> Point * Vector