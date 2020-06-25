namespace Shapes

module Triangle =
    
    open System
    open System.Drawing
    open Util
    open Ray
    open Point
    open Vector
    open Types
    open BoundingBox

    type BaseTriangle(pa:Point, pb:Point, pc:Point, vn: Vector option, vertexNormals : (Vector*Vector*Vector) option) =
        let (ax,ay,az) = Point.getCoord pa
        let (bx,by,bz) = Point.getCoord pb
        let (cx,cy,cz) = Point.getCoord pc
        let lx = Seq.min[ax;bx;cx]
        let ly = Seq.min[ay;by;cy]
        let lz = Seq.min[az;bz;cz]
        let hx = Seq.max[ax;bx;cx]
        let hy = Seq.max[ay;by;cy]
        let hz = Seq.max[az;bz;cz]
        let L = mkPoint lx ly lz
        let H = mkPoint hx hy hz
        let bbox = mkBoundingBox L H |> Some
        let a = ax-bx
        let b = ax-cx
        let e = ay-by
        let f = ay-cy
        let i = az-bz
        let j = az-cz
        let af = a*f
        let aj = a*j
        let bi = b*i
        let eb = e*b
        let fi = f*i
        let ej = e*j
        let ppDiscriminant c g k = af*k-aj*g+bi*g-eb*k-c*fi+ej*c    // expanded version of a*(f*k-g*j)+b*(g*i-e*k)+c*(e*j-f*i), in order to do a bit of preprocessing
        member this.bb = bbox
        member this.pa = pa
        member this.pb = pa
        member this.pc = pc
        interface IBaseShape with
            member this.isInside p = false
            member this.getUV p = (0.,0.) 
            member this.boundingBox = bbox             
            member this.hitFunction ray = 
                let o = Ray.getOrigin ray
                let _d = Ray.getDirection ray

                let (ox,oy,oz) = Point.getCoord o
                let (dx,dy,dz) = Vector.getCoord _d

                let c = dx
                let d = ax-ox
                let g = dy
                let h = ay-oy
                let k = dz
                let l = az-oz

                let disc = ppDiscriminant c g k

                match disc with
                | 0. -> None
                | D -> 
                    let beta = (d*(f*k-g*j)+b*(g*l-h*k)+c*(h*j-f*l))/D
                    let gamma = (a*(h*k-g*l)+d*(g*i-e*k)+c*(e*l-h*i))/D
                    let t = (a*(f*l-h*j)+b*(h*i-e*l)+d*(e*j-f*i))/D
                    let alpha = 1.-beta-gamma

                    if beta > 0. && beta < 1. &&
                       gamma > 0. && beta < 1. &&
                       beta + gamma <= 1. && t > 0.
                    then 
                        match vn with 
                        | Some(v) -> Some(t,v)
                        | None -> match vertexNormals with
                                    | Some(xn,xy,xz) -> Some(t,(alpha*xn+beta*xy+gamma*xz))
                                    | None -> failwith "Normals were not correctly setup, terminating.."
                    else None
            member this.shadowHitFunc ray =  
                match (this :> IBaseShape).hitFunction ray with 
                | Some (t,_)    -> Some t
                | None          -> None
                    

    type TexturedTriangle(pa:Point, pb:Point, pc:Point, vn: Vector option, vertexNormals : (Vector*Vector*Vector) option, texCoords : (float*float*float*float*float*float)) =
        let va, vb, vc, ua, ub, uc =
            match texCoords with
            | (va,vb,vc,ua,ub,uc) -> va, vb, vc, ua, ub, uc

        let (ax,ay,az) = Point.getCoord pa
        let (bx,by,bz) = Point.getCoord pb
        let (cx,cy,cz) = Point.getCoord pc
        let lx = Seq.min[ax;bx;cx]
        let ly = Seq.min[ay;by;cy]
        let lz = Seq.min[az;bz;cz]
        let hx = Seq.max[ax;bx;cx]
        let hy = Seq.max[ay;by;cy]
        let hz = Seq.max[az;bz;cz]
        let L = mkPoint lx ly lz
        let H = mkPoint hx hy hz
        let bbox = mkBoundingBox L H |> Some
        let a = ax-bx
        let b = ax-cx
        let e = ay-by
        let f = ay-cy
        let i = az-bz
        let j = az-cz
        let af = a*f
        let aj = a*j
        let bi = b*i
        let eb = e*b
        let fi = f*i
        let ej = e*j
        let ppDiscriminant c g k = af*k-aj*g+bi*g-eb*k-c*fi+ej*c    // expanded version of a*(f*k-g*j)+b*(g*i-e*k)+c*(e*j-f*i), in order to do a bit of preprocessing
        member this.bb = bbox
        member this.pa = pa
        member this.pb = pa
        member this.pc = pc
        interface IBaseShape with
            member this.getUV p = (0., 0.)
                
            member this.boundingBox = bbox
            member this.isInside p = false             
            member this.hitFunction ray = 
                let o,_d = Ray.getComponents ray

                let (ox,oy,oz) = Point.getCoord o
                let (dx,dy,dz) = Vector.getCoord _d

                let c = dx
                let d = ax-ox
                let g = dy
                let h = ay-oy
                let k = dz
                let l = az-oz

                let disc = ppDiscriminant c g k

                match disc with
                | 0. -> None
                | D -> 
                    let beta = (d*(f*k-g*j)+b*(g*l-h*k)+c*(h*j-f*l))/D
                    let gamma = (a*(h*k-g*l)+d*(g*i-e*k)+c*(e*l-h*i))/D
                    let t = (a*(f*l-h*j)+b*(h*i-e*l)+d*(e*j-f*i))/D
                    let alpha = 1.-beta-gamma

                    if beta > 0. && beta < 1. &&
                       gamma > 0. && beta < 1. &&
                       beta + gamma <= 1. && t > 0.
                    then 
                        match vn with 
                        | Some(n) -> Some(t,n)
                        | None -> match vertexNormals with
                                    | Some(xn,xy,xz) -> Some(t,(alpha*xn+beta*xy+gamma*xz))
                                    | None -> failwith "Normals were not correctly setup, terminating.."
                    else None

            member this.shadowHitFunc ray = 
                match (this :> IBaseShape).hitFunction ray with 
                | Some (t,_)    -> Some t
                | None          -> None


    type Triangle(bt:BaseTriangle, material:IMaterial) =
        let (ax,ay,az) = Point.getCoord bt.pa
        let (bx,by,bz) = Point.getCoord bt.pb
        let (cx,cy,cz) = Point.getCoord bt.pc
        member this.getUV p =
            (bt :> IBaseShape).getUV p
        interface IShape with
            member this.boundingBox = (bt :> IBaseShape).boundingBox
            member this.isInside p = false
            member this.shadowHitFunc ray = 
                let t = (bt :> IBaseShape).shadowHitFunc ray
                t
            member this.hitFunction ray =
                let hit = (bt :> IBaseShape).hitFunction ray
                match hit with
                | None -> None
                | Some(t, n) -> 
                    let o = getOrigin ray
                    let d = getDirection ray
                    let p = o + t * d
                    Some(t, n, material)  
                        
        
    let mkTriangle a b c m = 
        let u = a-b
        let v = c-a
        let norm = Vector.normalise (Vector.crossProduct u v)
        let bt = BaseTriangle(a,b,c, Some(norm), None)
        Triangle(bt, m)
        
    