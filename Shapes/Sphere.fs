namespace Shapes

module Sphere =

    open System
    open Util
    open Point
    open Vector
    open Ray
    open Types
    open BoundingBox
    open BaseSphere
    open Transformations.Transformation
    open Shape

    type Sphere(bs:BaseSphere, texture:ITexture, invNormal:bool) =
        let radius = bs.radius
        member this.getNormal p = if invNormal then -bs.getNormal p else bs.getNormal p
        member this.getUV p =
            (bs :> IBaseShape).getUV p
        member this.getMaterial p = 
            let (u,v) = this.getUV p
            texture.getMaterial u v
        interface IShape with
            member this.boundingBox = (bs :> IBaseShape).boundingBox
            member this.isInside p =  (bs :> IBaseShape).isInside p
            member this.shadowHitFunc ray = (bs :> IBaseShape).shadowHitFunc ray
            member this.hitFunction ray = 
                let hit = (bs :> IBaseShape).hitFunction ray
                match hit with
                | None -> None
                | Some(t, n) ->
                    let (o,d) = Ray.getComponents ray
                    let p = o + t * d
                    let n' = if invNormal then -n else n
                    Some(t, n', this.getMaterial p)


    let mkSphere c (r:float) t =
        let bs = BaseSphere(r)
        let s = Sphere(bs, t, false)
        let c = Point.getCoord c
        if c = (0., 0., 0.)
        then s :> IShape
        else
            let translate = translate c
            transform s translate

    let mkEnviSphere c (r:float) t =
         let bs = BaseSphere(r)
         let s = Sphere(bs, t, true)
         s