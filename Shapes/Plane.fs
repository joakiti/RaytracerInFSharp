namespace Shapes

open Util
open Types
open Ray
open Vector
open Point
open BoundingBox

module Plane =

    type Plane(texture:ITexture) =
        member this.getNormal p = mkVector 0. 0. 1.
        member this.getUV p =
            let (px, py, _) = Point.getCoord p
            let u = px
            let v = py
            (u,v)
        interface IShape with
            member this.isInside p = false
            member this.boundingBox = None
            member this.shadowHitFunc ray = 
                let (o,d) = getComponents ray
                let (_,_,dz) = Vector.getCoord d
                let (_,_,oz) = Point.getCoord o

                match dz with
                | 0.    ->  None
                | _     ->  let t = -oz/dz
                            if t > 0. then Some t
                            else None
            member this.hitFunction ray = 
                let o,d = getComponents ray
                let t = (this :> IShape).shadowHitFunc ray
                match t with
                | Some t ->
                   let n = mkVector 0. 0. 1.
                   let p = o + t * d
                   let p' = move p n
                   let (u,v) = this.getUV p
                   let m = texture.getMaterial u v
                   Some(t,n,m)
                | None -> None


