namespace Shapes

module Box =
    
    open Util
    open Point
    open BoundingBox
    open Types
    open Ray
    open Vector

    let pf = [|Point.getX;Point.getY;Point.getZ|]
    let vf = [|Vector.getX;Vector.getY;Vector.getZ|]


    type Box(L:Point, H:Point, front:ITexture, back:ITexture, top:ITexture, bottom:ITexture, left:ITexture, right:ITexture) = 
        let (lx, ly, lz) = Point.getCoord L
        let (hx, hy, hz) = Point.getCoord H
        let width = hx - lx
        let height = hy - ly
        let depth = hz - lz               
        interface IShape with
            member this.boundingBox = 
                Some(mkBoundingBox L H)
            member this.shadowHitFunc ray = None // TODO denne skal returnere distancen til hittet
            member this.hitFunction ray = 

                let (o,d) = getComponents ray
                let (dx,dy,dz) = Vector.getCoord d
        
                // HELPER: calc the rays distance to (L.x, H.x) or (L.y, H.y) or (L,z, H.z) depending on the axis
                let rayDist ax =             
                    let t = (pf.[ax] L - pf.[ax] o) / vf.[ax] d     // t = distance to entrance of boundingbox
                    let t' = (pf.[ax] H - pf.[ax] o) / vf.[ax] d    // t' = distance to exit
                    
                    if (vf.[ax] d >= 0.) then (t, t') else (t',t)              // if r goes normal direction then (L=distToEntrance, H=distToExit)...
                
                let (tx, tx') = rayDist 0  // distance to (lx,hx)
                let (ty, ty') = rayDist 1  // distance to (ly,hy)
                let (tz, tz') = rayDist 2  // distance to (lz,hz)
                
                let t = List.max ([tx; ty; tz])         //the ray enters the box at max(tx, ty, tz)
                let t' = List.min ([tx'; ty'; tz'])     //the ray exits the box at min(tx', ty', tz')
                

                let hit = (t < t') && (t' > 0.)         //(ray must enter box before exit) && (exit must not be behind origin of ray)
                match hit with
                | true ->
                    if t > 0. 
                    then // ray hits box from outside
                        let p = o + t * d
                        let (px,py,pz) = Point.getCoord p
                        if t = tx
                            then 
                                let u = pz / depth
                                let v = py / height
                                if dx > 0.
                                then // ray hits left face
                                    let n = mkVector -1. 0. 0.
                                    let m = left.getMaterial u v
                                    Some(t,n,m)
                                else // ray hits right face
                                    let n = mkVector 1. 0. 0.
                                    let m = right.getMaterial u v
                                    Some(t,n,m)

                        elif t = ty
                            then
                                let u = pz / depth
                                let v = px / width
                                if dy > 0.
                                then // ray hits bottom face
                                    let n = mkVector 0. -1. 0.
                                    let m = bottom.getMaterial u v
                                    Some(t,n,m)
                                else // ray hits top face
                                    let n = mkVector 0. 1. 0.
                                    let m = top.getMaterial u v
                                    Some(t,n,m)
                        
                        else
                                let u = px / width
                                let v = py / height
                                if dz > 0.
                                then // ray hits back face
                                    let n = mkVector 0. 0. -1.
                                    let m = back.getMaterial u v
                                    Some(t,n,m)
                                else // ray hits front face
                                    let n = mkVector 0. 0. 1.
                                    let m = front.getMaterial u v
                                    Some(t,n,m)

                    else //box is hit from inside
                        let p = o + t' * d
                        let (px,py,pz) = Point.getCoord p
                        if t' = tx'  
                            then 
                                let u = pz / depth
                                let v = py / height
                                if dx > 0.
                                then // ray hits right face
                                    let n = mkVector 1. 0. 0.
                                    let m = right.getMaterial u v
                                    Some(t',n,m)
                                else // ray hits left face
                                    let n = mkVector -1. 0. 0.
                                    let m = left.getMaterial u v
                                    Some(t',n,m)


                        elif t' = ty'
                            then
                                let u = pz / depth
                                let v = px / width
                                if dz > 0.
                                then // ray hits top face
                                    let n = mkVector 0. 1. 0.
                                    let m = top.getMaterial u v
                                    Some(t',n,m)
                                else // ray hits bottom face
                                    let n = mkVector 0. -1. 0.
                                    let m = bottom.getMaterial u v
                                    Some(t',n,m)
                        
                        else
                                let u = px / width
                                let v = py / height
                                if dz > 0.
                                then // ray hits front face
                                    let n = mkVector 0. 0. 1.
                                    let m = front.getMaterial u v
                                    Some(t',n,m)
                                else // ray hits back face
                                    let n = mkVector 0. 0. -1.
                                    let m = back.getMaterial u v
                                    Some(t',n,m)
                | false -> None

            member this.isInside p = 
                let (x,y,z) = Point.getCoord p
                x >= lx && x <= hx &&
                y >= ly && y <= hy &&
                z >= lz && z <= hz
                