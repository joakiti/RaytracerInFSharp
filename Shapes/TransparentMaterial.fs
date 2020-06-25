namespace Shapes

module TransparentMaterial =
    
    open System
    open System.Drawing
    open Util
    open Colour
    open Point
    open Vector
    open Ray
    open Types

    let black = mkColour 0. 0. 0.
    let white = mkColour 1. 1. 1.

    type TransparentMaterial(cf_in:Colour, cf_out:Colour, eta_in:float, eta_out:float) =
        interface IMaterial with
            member this.colour sc d p n depth = 
                let maxDepth = sc.m
                let dn = d * n > 0.
                let cf_in, cf_out, eta_in, eta_out =
                    if dn
                    then cf_out, cf_in, eta_out, eta_in
                    else cf_in, cf_out, eta_in, eta_out
                let n = 
                    if dn
                    then -n
                    else n
                let e = sc.eps
                let eta = eta_in / eta_out // Relative index of refraction
                let cosI = -(n*d)
                let cosTRoot = 1.-(1.-(Math.Cos cosI)**2.)/(eta**2.)
                let cosT = 
                    if cosTRoot > 0.
                    then Math.Sqrt cosTRoot // transmission, ie. ray goes through material
                    else 0. // total internal reflection, ie. ray is perfectly reflected

                let kr = // fraction of light being reflected
                    match cosT with
                    | 0. -> 1.
                    | ct -> let rOr = (eta * cosI - ct) / (eta * cosI + ct) // bedre navn? Hvad beskriver disse?
                            let rNot = (cosI - eta * ct) / (cosI + eta * ct) // ???
                            let kr' = ((rOr**2. + rNot**2.) / 2.)
                            if kr' > 1.
                            then 1.
                            else kr'
                
                let kt = 
                    match kr with
                    |1. -> 0.
                    |_ ->  1. - kr // fraction of light being transmitted
                
                // send rays for both transmission and reflection. Multiply resulting color by respective kr / kt
                let dr = d+(-2.*(n * d))*n // reflected ray direction
                let dt = ((1. / eta) * d) - ((cosT - (cosI / eta)) * n) // refracted ray direction

                let pr = p + e * n
                let pt = p + e * -n

                let reflectRay = mkRay pr dr
                let transmitRay = mkRay pt dt

                let rHit = sc.hitFunction reflectRay
                let tHit = sc.hitFunction transmitRay

                if depth < maxDepth
                then
                    let cr,tr = 
                        match rHit with
                        | None -> black, 0.
                        | Some(t,n,m) ->
                            let p' = pr + t * dr
                            m.colour sc dr p' n (depth+1), t
                
                    let ct,tt = 
                        match tHit with
                        | None -> black, 0.
                        | Some(t,n,m) ->
                            let p' = pt + t * dt
                            m.colour sc dt p' n (depth+1), t


                    kr * ((cf_out^^tr) * cr) + kt/(eta**2.) * ((cf_in^^tt) * ct)
                else 
                    white