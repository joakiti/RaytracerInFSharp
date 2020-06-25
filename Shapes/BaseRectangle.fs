namespace Shapes

module BaseRectangle =

    open Util
    open Point
    open Vector
    open Types
    open Util.BoundingBox

    type BaseRectangle(topLeft:Point, bottomRight:Point) =
        let bottomLeft = mkPoint 0. 0. 0.
        member this.topLeft = topLeft
        member this.bottomRight = bottomRight
        member this.width = magnitude (bottomRight-bottomLeft)
        member this.height = magnitude (topLeft-bottomLeft)
        member this.getNormal = Vector.mkVector 0. 0. 1.
        interface IBaseShape with
            member this.isInside p = false 
            member this.getUV p = 
                let u = (Point.getX p)/this.width
                let v = (Point.getY p)/this.height
                (u,v)
            member this.boundingBox =
                let L = bottomLeft
                let H = Point.mkPoint (Point.getX bottomRight) (Point.getY topLeft) 0.
                mkBoundingBox L H |> Some
            member this.shadowHitFunc ray = 
                 let o = Ray.getOrigin ray
                 let oz = Point.getZ o
 
                 let d = Ray.getDirection ray
                 let dz = Vector.getZ d
                 
                 let t = 
                     match dz with
                     | 0. -> 0.
                     | _ -> -oz/dz
 
                 match t with
                 | t when t <= 0. -> None
                 | _ ->
                     let px = Point.getX o + t * Vector.getX d
                     let py = Point.getY o + t * Vector.getY d
 
                     let hit = 0. <= px && px <= this.width && 
                                 0. <= py && py <= this.height
                     if hit
                     then let pz = 0.
                          let p = Point.mkPoint px py pz
                          let dist = Vector.magnitude (Point.distance p o)
                          Some(dist)
                     else None
            member this.hitFunction ray = 
                let t = (this :> IBaseShape).shadowHitFunc ray
                match t with 
                | Some t -> Some(t,this.getNormal)
                | None -> None
