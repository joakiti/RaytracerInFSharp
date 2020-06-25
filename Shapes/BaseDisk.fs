namespace Shapes

module BaseDisk =

    open Util
    open Point
    open Vector
    open Types
    open Util.BoundingBox

    type BaseDisk(r:float) =
        member this.center = mkPoint 0. 0. 0.
        member this.radius = r
        member this.getNormal = mkVector 0. 0. 1.
        interface IBaseShape with
            member this.isInside p = false
            member this.getUV p = 
                let(px,py,_) = Point.getCoord p
                let u = (px+r)/(2.*r)
                let v = (py+r)/(2.*r)
                (u, v)
            member this.boundingBox =
                let L = mkPoint -r -r 0.
                let H = mkPoint r r 0.
                mkBoundingBox L H |> Some
            member this.shadowHitFunc ray = 
                let o = Ray.getOrigin ray
                let oz = Point.getZ o
    
                let d = Ray.getDirection ray
                let dz = Vector.getZ d
                
                let t = 
                    match dz with
                    | 0. -> 0.
                    | dz -> -oz/dz
    
                match t with
                | t when t <= 0. -> None
                | t when t > 0. ->
                    let p = o + t * d
                    let (px,py,_) = Point.getCoord p
    
                    let hit = px**2.+py**2. <= r**2.
                    if hit
                    then 
                         Some(t)
                    else None
            member this.hitFunction ray = 
                let t = (this:>IBaseShape).shadowHitFunc ray
                match t with
                | Some t    -> Some(t,this.getNormal)
                | None      -> None
