namespace Util

module Point =

    open Vector
    open System

    type Vector = Vector.Vector
    type Point =
      |P of float * float * float
      override p.ToString() =
        match p with
          P(x,y,z) -> "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"

    let mkPoint x y z = P(x,y,z)
    let getX (P(x,_,_)) = x
    let getY (P(_,y,_)) = y
    let getZ (P(_,_,z)) = z
    let getCoord (P(x,y,z)) = (x,y,z)
    let move (P(px,py,pz)) (v: Vector) = P(px+Vector.getX(v), py+Vector.getY(v), pz+Vector.getZ(v)) 
    let distance (P(px,py,pz)) (P(qx,qy,qz)) = Vector.mkVector (qx-px) (qy-py) (qz-pz)
    let direction p q = normalise(distance p q)
    let round (P(px,py,pz)) (d:int) = P(Math.Round(px, d), Math.Round(py, d), Math.Round(pz, d))

    type Point with
      static member ( + ) (P(x,y,z), v) : Point = P(x+Vector.getX(v), y+Vector.getY(v), z+Vector.getZ(v))
      static member ( - ) (P(px,py,pz), P(qx,qy,qz)) : Vector = mkVector (px-qx) (py-qy) (pz-qz)

    let D v (w:Point) = w - v //calculates the distance vector between two points