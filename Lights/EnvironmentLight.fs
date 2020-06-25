namespace Lights

module EnvironmentLight =

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
    open Shapes.Sphere
           
    type EnvironmentLight(radius:float, tex:ITexture, s:Sampler, sphere:IShape) =                
                               
        interface ILight with
            // lc fires a shadow ray from the hitpint through the sample point. 
            // if the shadow ray hits the sphere, the colour from the emissive material is calculated.
            member this.lc p n sc sp p' = 
                            let dn = (this :> ILight).ld p n sp 
                            let sr = mkRay p dn                            
                            match (sphere.hitFunction sr) with
                            | Some (t,_,m) ->
                                    let p' = getPointOnRay t sr
                                    let sn = match sphere with 
                                             | :? Sphere -> (sphere :?> Sphere).getNormal p'
                                             | _ -> failwith "Not a sphere"
                                    m.colour sc dn p' sn 1 
                            | None -> mkColour 0. 0. 0.
            member this.ld p n sp = 
                            let up = Vector.mkVector 0.00034 1. 0.00086 // pointing upwards in the scene
                            let frame = camCoords n up // creates an orthonormal coordinate frame from n and up
                            let spw = applyOrthoFrameToVec frame sp
                            Vector.normalise(spw)
            // to check if the light is blocked, a ray is fired from the hitpoint
            // of the shape, with direction through the sample point.
            member this.ls p n sc sp =
                            let dirNorm = (this :> ILight).ld p n sp
                            let shadowRay = mkRay p dirNorm
                            match sc.shadowHit shadowRay with 
                                       | Some _ -> true
                                       | None -> false                           
            member this.lg p sp = 1.
            member this.lpdf p n sp = ((n * (this :> ILight).ld p n sp))/Math.PI
            // SP returns the sample point from the sampler, to be able to use the same sample point when
            // the functions are called. Takes a point, p, because it is needed for the arealight, when
            // it is a sphere.
            member this.SP p = 
                let (x,y,z) = s.getNextUH
                mkPoint x y z
            // isShape and getShape is used to check if the light has a shape, and if so add it to the scene.                
            member this.isShape = true
            member this.getShape = sphere
    
    let mkEnvironmentLight radius tex s =
        let sphere = mkEnviSphere (mkPoint 0. 0. 0.) radius tex :> IShape
        let uh = mapPointsToUH s 0.
        EnvironmentLight(radius, tex, uh, sphere)
        