namespace Shapes

module MatteMaterial =
    open System
    open System.Drawing
    open Util 
    open Types
    open Sampling 
    open Sampler
    open Orthonormal 
    open Point
    open Vector
    open Ray
    open Colour

    let black = mkColour 0. 0. 0.   

    type MatteMaterial(ca:Colour, ka:float, cd:Colour, kd:float) =
        let kaca = ka*ca
        let kdcdpi = 1./Math.PI * kd * cd
        interface IMaterial with
            member this.colour sc d p n depth =
                let la = sc.la
                let lss = sc.lss
                let eps = sc.eps
                let n = 
                    if n * d > 0.
                    then -n
                    else n
                let p' = move p (eps * n)
                let kacalac = kaca * la.lc p' n sc
                let lightColours = mkColour 0. 0. 0.
                let calcLight (l:ILight) =
                    let sp = l.SP p' 
                    let ld = l.ld p' n sp
                    let lg = 1. // l.lg p' sp
                    let lpdf = 1. //l.lpdf p' n sp
                    if (l.ls p' n sc sp = false && ld * n > 0.)
                    then kdcdpi * (lg/lpdf * (n * ld) * (l.lc p' n sc sp p'))
                    else black
                let rec colour acc = function
                | [] -> kacalac + acc
                | l::lss -> colour (acc + calcLight l) lss
                colour lightColours lss

    type MatteReflectiveMaterial(ca:Colour, ka:float, cd:Colour, kd:float, cr:Colour, kr:float) =
        let kaca = ka*ca
        let kdcdpi =  1./Math.PI * kd * cd
        interface IMaterial with
            member this.colour sc d p' n depth = 
                let la = sc.la
                let lss = sc.lss
                let eps = sc.eps
                let maxDepth = sc.m
                let n = 
                    if n * d > 0.
                    then -n
                    else n
                let p' = move p' (eps * n)
                let kacalac = kaca * la.lc p' n sc
                let lightColours = mkColour 0. 0. 0.
                let calcLight (l:ILight) =
                    let sp = l.SP p' 
                    let ld = l.ld p' n sp
                    let lg = 1. // l.lg p' sp
                    let lpdf = 1. // l.lpdf p' n sp
                    if (l.ls p' n sc sp = false && ld * n > 0.)
                    then kdcdpi * (lg/lpdf * (n * ld) * (l.lc p' n sc sp p'))
                    else black
                let rec colour acc = function
                | [] -> kacalac + acc
                | l::lss -> colour (acc + calcLight l) lss

                let c = colour lightColours lss
                if depth < maxDepth
                    then 
                        let d' = d + (-2. * (n * d) * n) // Direction of reflected ray
                        let ray' = mkRay p' d'
                        let hit = sc.hitFunction ray'
                        match hit with
                        | None -> c
                        | Some(t,n,m) ->
                            let (o,d) = getComponents ray'
                            let p = o + t * d
                            let c' = m.colour sc d p n (depth+1)
                            c + kr * cr * c'
                    else c
                    
    type MatteGlossyReflectiveMaterial(ca:Colour, ka:float, cd:Colour, kd:float, cr:Colour, kr:float, exps:int, s:Sampler) =        
        let kaca = ka*ca
        let kdcdpi =  1./Math.PI * kd * cd
        interface IMaterial with
            member this.colour sc d p' n depth = 
                let la = sc.la
                let lss = sc.lss
                let eps = sc.eps
                let maxDepth = sc.m                
                let n = 
                    if n * d > 0.
                    then -n
                    else n
                let p' = move p' (eps * n)
                let kacalac = kaca * la.lc p' n sc
                let lightColours = mkColour 0. 0. 0.
                let calcLight (l:ILight) =
                    let sp = l.SP p'
                    let ld = l.ld p' n sp
                    let lg = 1. // l.lg p' sp
                    let lpdf = 1. // l.lpdf p' n sp
                    if (l.ls p' n sc sp = false && ld * n > 0.)
                    then kdcdpi * (lg/lpdf * (n * ld) * (l.lc p' n sc sp p'))
                    else black
                let rec colour acc = function
                | [] -> kacalac + acc
                | l::lss -> colour (acc + calcLight l) lss

                let c = colour lightColours lss
                if depth < maxDepth
                    then  
                        let sp = 
                            let (x,y,z) = (getNextUH s)
                            mkPoint x y z                                  
                        // the reflected ray is found by mirroring the ingoing ray around the normal og the hitpoint.
                        // the orthonomal frame is set up using this ray direction.
                        let mVec = (d + 2.0 * (n * -d) * n)
                        let f = camCoords mVec (mkVector 0. 1. 0.)
                        let (u,v,w) = f
                        // calculation of the outgoing reflective vector by using the orthonomal frame on sp
                        let ofv = applyOrthoFrameToVec f sp
                        let d' = if ofv * n > 0.  
                                    then ofv 
                                    else (-(Point.getX sp) * u - (Point.getY sp) * v + (Point.getZ sp) * w)
                                                         
                        let ray' = mkRay p' d'
                        let hit = sc.hitFunction ray'
                        match hit with
                        | None -> c
                        | Some(t,n,m) ->
                            let (o,d) = getComponents ray'
                            let p = o + t * d
                            let c' = m.colour sc d p n (depth+1)
                            c + kr * cr * c'
                    else c
    
    let mkMatteGlossyReflectiveMaterial(ca:Colour, ka:float, cd:Colour, kd:float, cr:Colour, kr:float, exps:int, s:Sampler) =
        let uh = mapPointsToUH s (float exps)
        MatteGlossyReflectiveMaterial(ca, ka, cd, kd, cr, kr, exps, uh)
