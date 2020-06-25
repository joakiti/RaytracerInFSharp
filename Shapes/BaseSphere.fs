namespace Shapes

module BaseSphere =
    open System
    open Util
    open Point
    open Types
    open Util.BoundingBox
    open Util.Vector
    open Util.Ray

    type BaseSphere(radius:float) =
        let center = mkPoint 0. 0. 0.
        member this.radius = radius
        member this.getNormal p = 
            let px,py,pz = Point.getCoord p
            1./radius * Vector.mkVector px py pz
        interface IBaseShape with
            member this.isInside p = 
                let (px,py,pz) = Point.getCoord p
                px**2. + py**2. + pz**2. < radius**2.
            member this.getUV p = 
                let (x, y, z) = Point.getCoord p
                let (nx, ny, nz) = ((x/radius), (y/radius), (z/radius))
                let lon' = Math.Atan2(nx, nz)
                let lon = 
                    if lon' < 0.
                    then lon' + 2. * Math.PI
                    else lon'
                let lat = Math.Acos (ny)

                let u = lon / (2. * Math.PI)
                let v = (1. - lat) / Math.PI
                (u,v)
            member this.boundingBox =
                let px = Point.getX center
                let py = Point.getY center
                let pz = Point.getZ center
                let L = Point.mkPoint (px-radius) (py-radius) (pz-radius)
                let H = Point.mkPoint (px+radius) (py+radius) (pz+radius)
                mkBoundingBox L H |> Some// Fix me!
            member this.shadowHitFunc ray =
                let o,d = Ray.getComponents ray
                let ox,oy,oz = Point.getCoord o    // get each coordinate for origin point of the ray
                let dx,dy,dz = Vector.getCoord d  // get each coordinate for direction of the ray
                
                // calculate a b c for calculating the discriminant
                let a = dx**2.+dy**2.+dz**2.
                let b = 2.*(ox*dx+oy*dy+oz*dz)
                let c = ox**2.+oy**2.+oz**2.-radius**2.

                let D = b**2.-4.*a*c
                match D with // hvorfor siger den incomplete pattern matching? fordi det er en dum compiler
                | q when q < 0.0 -> None
                | q when q = 0.0 -> 
                    let t = -b/2.*a
                    Some t
                | D  -> 
                    let t1 = (-b + System.Math.Sqrt(D))/(2.*a)
                    let t2 = (-b - System.Math.Sqrt(D))/(2.*a)
                    if t1 > 0.0 && t2 > 0.0 
                    then 
                        let t = min t1 t2
                        Some(t)
                    elif t1 > 0.0 && t2 < 0.0
                    then 
                        Some(t1)
                    elif t2 > 0.0 && t1 < 0.0
                    then 
                        Some(t2)
                    else None
                
            member this.hitFunction ray = 
                let o,d = Ray.getComponents ray
                let ox,oy,oz = Point.getCoord o    // get each coordinate for origin point of the ray
                let dx,dy,dz = Vector.getCoord d  // get each coordinate for direction of the ray
                // calculate a b c for calculating the discriminant
                let a = dx**2.+dy**2.+dz**2.
                let b = 2.*(ox*dx+oy*dy+oz*dz)
                let c = ox**2.+oy**2.+oz**2.-radius**2.

                let D = b**2.-4.*a*c

                match D with
                | q when q < 0.0 -> None
                | q when q = 0.0 -> 
                    let t = -b/2.*a
                    let p = o + t * d
                    Some(t, this.getNormal p)
                | D -> 
                    let t1 = (-b + System.Math.Sqrt(D))/(2.*a)
                    let t2 = (-b - System.Math.Sqrt(D))/(2.*a)
                    if t1 > 0.0 && t2 > 0.0
                    then let t = min t1 t2
                         let p = o + t * d
                         Some(t, this.getNormal p)
                    elif t1 > 0.0 && t2 < 0.0
                    then 
                        let p = o + t1 * d
                        Some(t1, this.getNormal p)
                    elif t2 > 0.0 && t1 < 0.0
                    then 
                        let p = o + t1 * d
                        Some(t1, this.getNormal p)
                    else None