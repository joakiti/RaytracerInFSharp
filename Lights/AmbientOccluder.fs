namespace Lights

module AmbientOccluder =
    
    open Util
    open System
    open Sampling
    open Sampler
    open Point
    open Colour
    open Vector
    open Orthonormal
    open Ray 
    open Types
                
    type AmbientOccluder(colour:Colour, intensity:float, min_intensity:float, s:Sampler) =
        // the upvector for the orthonomal frame.
        member this.up = Vector.mkVector 0.00086 1. 0.00034 

        interface IAmbientLight with
            member this.lc p n sc =
                          // the orthonormal frame is made from the normal of the hitpoint n and the up. 
                          let frame = camCoords n this.up  
                          let sp = 
                                let (x,y,z) = s.getNextUH 
                                mkPoint x y z
                          // the sample point is transformed to hemisphere on normal of the shape.
                          let spw = applyOrthoFrameToVec frame sp
                          let dn = Vector.normalise(spw)                           
                          let isBlocked = 
                                   // to check if the light is blocked, a ray is fired from the hitpoint
                                   // of the shape, with direction through the sample point.
                                   let ray = mkRay p dn
                                   match sc.shadowHit ray with 
                                   | Some _ -> true
                                   | None -> false
                          if isBlocked then min_intensity * (intensity * colour) else (intensity * colour)
    
    let mkAmbientOccluder c f1 f2 s = 
        let uh = mapPointsToUH s 1.
        AmbientOccluder(c, f1, f2, uh)