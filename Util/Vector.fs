namespace Util

module Vector =

    type Vector =
      | V of float * float * float
      override v.ToString() =
        match v with
          V(x,y,z) -> "["+x.ToString()+","+y.ToString()+","+z.ToString()+"]"

    let mkVector x y z = V(x,y,z)
    let getX (V(x,_,_)) = x
    let getY (V(_,y,_)) = y
    let getZ (V(_,_,z)) = z
    let getCoord (V(x,y,z)) = (x,y,z)
    let multScalar s (V(x,y,z)) = V(s*x,s*y,s*z)
    let magnitude (V(x,y,z)) = System.Math.Sqrt(x*x + y*y + z*z)
    let dotProduct (V(ux,uy,uz)) (V(vx,vy,vz)) = ((ux*vx)+(uy*vy)+(uz*vz))
    let crossProduct (V(ux,uy,uz)) (V(vx,vy,vz)) = V(uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx)
    let normalise (V(x,y,z) as v) =
      let l = magnitude(v)
      V(x/l, y/l, z/l)
    let round (V(x,y,z)) (d:int) = V(System.Math.Round(x,d),System.Math.Round(y,d),System.Math.Round(z,d))

    type Vector with
      static member ( ~- ) (V(x,y,z)) = V(-x,-y,-z)
      static member ( + ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux+vx,uy+vy,uz+vz)
      static member ( - ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux-vx,uy-vy,uz-vz)
      static member ( * ) (s:float, v:Vector) = multScalar s v
      static member ( * ) (u, v) = dotProduct u v
      static member ( % ) (u, v) = crossProduct u v
      static member ( / ) (v, f) = multScalar (1.0 / f) v