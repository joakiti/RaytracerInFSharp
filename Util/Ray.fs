namespace Util

module Ray = 

    open Point
    open Vector

    type Vector = Vector.Vector
    type Point = Point.Point
    
    type Ray =
        | R of o:Point * v:Vector
        override r.ToString() =
           match r with
              R(o,d) -> "o = " + o.ToString() + "\n" + "d = " + d.ToString();
    
    let mkRay o d = R(o,d)
    let getOrigin (R(o,d)) = o
    let getDirection (R(o,d)) = d
    let getComponents (R(o,d)) = (o,d)
    // gets the point on the ray at length t (vector length).
    let getPointOnRay t (R(o,d)) = o+t*(normalise(d)) 