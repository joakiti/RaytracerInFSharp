namespace Util

module BoundingBox =

    open Point
    open Vector
    open Ray
    open Types
    open Point

    let pf = [|Point.getX; Point.getY; Point.getZ|]
    let vf = [|Vector.getX; Vector.getY; Vector.getZ|]
    
    type BoundingBox(L, H) =
        let w = Point.getX H - Point.getX L
        let h = Point.getY H - Point.getY L
        let d = Point.getZ H - Point.getZ L
        
        member this.L = (this :> IBoundingBox).L
        member this.H = (this :> IBoundingBox).H
        interface IBoundingBox with
            member this.L = L
            member this.H = H
            member this.Intersects ray : option<float*float> = 
                // HELPER: calc the rays distance to (L.x, H.x) or (L.y, H.y) or (L,z, H.z) depending on the axis
                let rayDist ax = 
                    let (o,d) = getComponents ray         // helper: extract o and d from the ray
                    let p = pf.[ax]
                    let v = vf.[ax]
            
                    let t = (p this.L - p o) / v d     // t = distance to entrance of boundingbox
                    let t' = (p this.H - p o) / v d    // t' = distance to exit
                    
                    if (v d >= 0.) then (t, t') else (t',t)              // if r goes normal direction then (L=distToEntrance, H=distToExit)...
                
                let (tx, tx') = rayDist 0  // distance to (lx,hx)
                let (ty, ty') = rayDist 1  // distance to (ly,hy)
                let (tz, tz') = rayDist 2  // distance to (lz,hz)
                
                let t = Array.max ([|tx; ty; tz|])         //the ray enters the box at max(tx, ty, tz)
                let t' = Array.min ([|tx'; ty'; tz'|])     //the ray exits the box at min(tx', ty', tz')
                
                let hit = (t < t') && (t' > 0.)         //(ray must enter box before exit) && (exit must not be behind origin of ray)
                if hit then Some(t, t') else None       //hit=true: return distance to entrance and exit of box 
            member this.width = w
            member this.height = h
            member this.depth = d
    
    
    let mkBoundingBox L H = 
        let e = 0.000001
        let (lx, ly, lz) = getCoord L
        let (hx, hy, hz) = getCoord H
        let L' = mkPoint (lx-e) (ly-e) (lz-e)
        let H' = mkPoint (hx+e) (hy+e) (hz+e)
        BoundingBox(L',H') :> IBoundingBox