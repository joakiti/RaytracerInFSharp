namespace Shapes

module Rectangle =

    open System
    open Util
    open Vector
    open Point
    open Ray
    open Types
    open BoundingBox
    open BaseRectangle
    open Transformations.Transformation
    open Shape

    type Rectangle(br:BaseRectangle, texture:ITexture) =
        let bottomLeft = mkPoint 0. 0. 0.
        let topLeft = br.topLeft
        let bottomRight = br.bottomRight
        let getUV p = (br :> IBaseShape).getUV p
        member this.width = br.width
        member this.height = br.height
        member this.getMaterial p ray = //Mulig optimering, kun returner matte material ikke udregn u v
            let (u,v) = getUV p
            texture.getMaterial u v
        interface IShape with
            member this.boundingBox = (br :> IBaseShape).boundingBox
            member this.isInside p = false //Can never be inside 2d shape
            member this.shadowHitFunc ray = (br :> IBaseShape).shadowHitFunc ray
            member this.hitFunction ray =
                let hit = (br :> IBaseShape).hitFunction ray
                match hit with
                | None -> None
                | Some(t, n) -> 
                    let o = getOrigin ray
                    let d = getDirection ray
                    let p = o + t * d
                    Some(t, n, this.getMaterial p ray)       

    let mkRectangle bl tl br texture =
        let o = (mkPoint 0. 0. 0.) - bl
        let tlo = move tl o
        let bro = move br o
        let brec = BaseRectangle(tlo, bro)

        let u = (normalise (br-bl))
        let v = (normalise (tl-bl))
        let w = u % v

        let obl = Point.getCoord bl
        let rot = orthRotate (u,v,w)
        let tr = translate obl
        let tf = mergeTransformations[rot;tr]

        transform (Rectangle(brec, texture)) tf

        