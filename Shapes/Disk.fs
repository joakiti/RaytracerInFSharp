namespace Shapes

module Disk =
    
    open System
    open Util
    open Vector
    open Point
    open Ray
    open Types
    open BoundingBox
    open BaseDisk
    open Transformations.Transformation
    open Shape

    type Disk(bd:BaseDisk, texture:ITexture) =
        member this.getUV p = (bd :> IBaseShape).getUV p
        member this.getMaterial p ray = 
            let (u,v) = this.getUV p
            texture.getMaterial u v
        interface IShape with
            member this.boundingBox = (bd :> IBaseShape).boundingBox
            member this.isInside p = false
            member this.shadowHitFunc ray = (bd :> IBaseShape).shadowHitFunc ray
            member this.hitFunction ray = 
                let hit = (bd :> IBaseShape).hitFunction ray
                match hit with
                | None -> None
                | Some(t, n) -> 
                    let o = getOrigin ray
                    let d = getDirection ray
                    let p = o + t * d
                    Some(t, n, this.getMaterial p ray)

    let mkDisk c r t =
        let bd = BaseDisk(r)
        let d = Disk(bd, t)
        let c = Point.getCoord c
        if c = (0., 0., 0.)
        then d :> IShape
        else let tl = translate c
             transform d tl

        