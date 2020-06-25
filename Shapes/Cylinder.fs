namespace Shapes

module Cylinder =
    open System
    open Util
    open Ray
    open Types
    open Point
    open Vector
    open BoundingBox
    open Transformations.Transformation
    open Disk
    open Shape

    type HollowCylinder(r:float, h:float, t:ITexture) =
        let center = mkPoint 0. 0. 0.
        member this.radius = r
        member this.height = h
        member this.getNormal p =
            let (px, _, pz) = Point.getCoord p
            mkVector (px/r) (0.) (pz/r)
        member this.getUV p =
            let (px, py, pz) = Point.getCoord p
            let (nx, _, nz) = (px/r, 0., pz/r)
            let lon' = Math.Atan2(nx, nz)
            let lon = if lon' < 0.
                        then lon' + 2. * Math.PI
                        else lon'
            let u = lon / (2. * Math.PI)
            let v = (py/h)+0.5
            (u,v)
        member this.getMaterial p =
            let (u,v) = this.getUV p
            t.getMaterial u v
        interface IShape with
            member this.boundingBox = 
                let L = mkPoint -r (-h/2.) -r
                let H = mkPoint r (h/2.) r
                Some(mkBoundingBox L H)
            member this.isInside p = 
                let (px,py,pz) = Point.getCoord p
                if px**2.+pz**2. <= r**2.
                then 
                    let py = Point.getY p
                    py >= -h/2. &&  py <= h/2.
                else false
            member this.shadowHitFunc ray = 
                let (o,d) = Ray.getComponents ray
                let (ox,oy,oz) = Point.getCoord o
                let (dx,dy,dz) = Vector.getCoord d
    
                // calculate a b c for calculating the discriminant
                let a = dx**2.+dz**2.
                let b = 2.*(ox*dx+oz*dz)
                let c = ox**2.+oz**2.-r**2.
    
                let D = b**2.-4.*a*c
                let t =     
                    match D with 
                    | 0. -> 
                        let t = -b/2.*a
                        if t > 0.0
                        then Some(t)
                        else None
                    | d' when d' < 0. -> None
                    | d' -> 
                        let t1 = (-b + System.Math.Sqrt(d'))/(2.*a)
                        let t2 = (-b - System.Math.Sqrt(d'))/(2.*a)
                        if t1 > 0.0 && t2 > 0.0
                        then 
                            let tmin = min t1 t2
                            let tmax = max t1 t2
                            let p = o + tmin * d
                            let py = Point.getY p
                            if py >= -h/2. &&  py <= h/2.
                            then Some(tmin)
                            else Some(tmax)
                        elif t1 > 0.0
                        then Some(t1)
                        elif t2 > 0.0
                        then Some(t2)
                        else None
                match t with
                | Some(t) ->
                    let o,d = Ray.getComponents ray
                    let p = o + t * d
                    let py = Point.getY p
                    if py >= -h/2. &&  py <= h/2.
                    then Some(t)
                    else None
                | None -> None
                    
            member this.hitFunction ray = 
                // Return hit option
                let t = (this :> IShape).shadowHitFunc ray
                match t with
                | Some(t) ->
                    let o,d = Ray.getComponents ray
                    let p = o + t * d
                    let n = this.getNormal p
                    let m = this.getMaterial p
                    Some(t, n, m)
                | None -> None


    type SolidCylinder(hc:HollowCylinder, tTex:ITexture, bTex:ITexture) =
        let center = mkPoint 0. 0. 0.
        let tDisk = mkDisk center hc.radius tTex
        let bDisk = mkDisk center hc.radius bTex

        let rt = rotateX (-Math.PI/2.)
        let tl = translate(0., hc.height/2., 0.)
        let tf = mergeTransformations [rt;tl]
        let top = transform tDisk tf

        let rt = rotateX(Math.PI/2.)
        let tl = translate(0., -hc.height/2., 0.)
        let tf = mergeTransformations [rt;tl]
        let bottom = transform bDisk tf

        interface IShape with
            member this.boundingBox = (hc :> IShape).boundingBox
            member this.isInside p = (hc :> IShape).isInside p 
            member this.shadowHitFunc ray = 
                let minimum t1 t2 =
                    if t1 < t2
                    then t1
                    else t2
                let hitC = (hc :> IShape).shadowHitFunc ray
                let hitT = top.shadowHitFunc ray
                let hitB = bottom.shadowHitFunc ray
                let hits = [hitC; hitT; hitB] |> List.choose id
                if hits.Length > 0
                then List.fold (fun closest hit -> minimum closest hit) hits.[0] hits |> Some
                else None
            member this.hitFunction ray = 
                let minimum (t1,n1,m1) (t2,n2,m2) =
                    if t1 < t2
                    then (t1,n1,m1)
                    else (t2,n2,m2)
                let hitC = (hc :> IShape).hitFunction ray
                let hitT = top.hitFunction ray
                let hitB = bottom.hitFunction ray
                let hits = [hitC; hitT; hitB] |> List.choose id
                if hits.Length > 0
                then List.fold (fun closest hit -> minimum closest hit) hits.[0] hits |> Some
                else None
                
    let mkHollowCylinder c r h t =
        let hc = HollowCylinder(r,h,t)
        let c = Point.getCoord c
        if c = (0., 0., 0.)
        then hc :> IShape
        else
            let translate = translate c
            transform hc translate

    let mkSolidCylinder c r h t tTex bTex =
        let hc = HollowCylinder(r,h,t)
        let sc = SolidCylinder(hc, tTex, bTex)
        let c = Point.getCoord c
        if c = (0., 0., 0.)
        then sc :> IShape
        else
            let translate = translate c
            transform sc translate
