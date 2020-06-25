namespace Lights

module AreaLight =

    open Util
    open System
    open Sampling
    open Point
    open Sampler
    open Colour
    open Vector
    open Orthonormal
    open Ray
    open Types
    open Shapes.BaseRectangle
    open Shapes.BaseDisk
    open Shapes.BaseSphere
    open Scene.Camera
    open Transformations.Transformation
    open Shapes.Shape
    open Shapes.Texture
    
    type AreaLight(arg, bs:IBaseShape, m:IMaterial) =
        let (sp,spn,spdf) = arg
        interface ILight with
            // returns the colour from the emissive material. 
            member this.lc p n sc sp p' =  
                            let dir = (Vector.normalise (Point.D sp p))
                            if (spn p') * dir > 0.
                            then m.colour sc -dir sp (spn p') 1
                            else mkColour 0. 0. 0.
            // the direction from the hitpoint, p, in the scene to the sample point, sp, on the shape
            // that is the light. 
            member this.ld p n sp = Vector.normalise(Point.D p sp)
            // checks if there is something in the scene, that intersects the hitpoint on a shape in the 
            // scene and the light shape.
            member this.ls p n sc sp = 
                            let dn = (this :> ILight).ld p n sp 
                            let sr = mkRay p dn
                            match sc.shadowHit sr with
                            | Some _ -> true
                            | None -> false
            member this.lg p sp =
                            let dspp = Point.D sp p
                            let top = ((spn p)) * Vector.normalise(dspp)
                            let bottom = dspp * dspp
                            top/bottom  
            member this.lpdf p n sp = spdf
            // SP returns the sample point from the sampler, to be able to use the same sample point when
            // the functions are called. Takes a point, p, because it is needed for the arealight, when
            // it is a sphere.            
            member this.SP p = sp p             
            // isShape and getShape is used to check if the light has a shape, and if so add it to the scene.
            member this.isShape = true
            member this.getShape = 
                    let tex = MatteTexture(m)
                    Shape(bs,tex) :> IShape
                                
     
    // the arguments sp, spn and spdf is calculated based on a match on the baseshape. 
    let mkAreaLight (b:IBaseShape) m s =
        match b with 
        | :? BaseRectangle -> 
            let w = (b :?> BaseRectangle).width 
            let h = (b :?> BaseRectangle).height
            let sp p = 
                let (x,y) = getNext s 
                mkPoint (x * w) (y * h) 0.
            let spn p = mkVector 0. 0. 1.
            let spdf = w * h
            AreaLight((sp,spn,spdf),b,m)
        | :? BaseDisk -> 
            let r = (b :?> BaseDisk).radius
            let sampler = s.mapPointsToUD
            let sp p = 
                let (x,y) = sampler.getNext
                mkPoint (x * r) (y * r) 0.
            let spn p = mkVector 0. 0. 1.
            let spdf = Math.PI * (r**2.)
            AreaLight((sp,spn,spdf),b,m)
        | :? BaseSphere -> 
            let c = mkPoint 0. 0. 0.
            let r = (b :?> BaseSphere).radius
            let uh = mapPointsToUH s 0.
            let spn p = Vector.normalise(Point.D c p)
            let sp p =
                let hem_sp = 
                     let (x,y,z) = getNextUH uh 
                     mkPoint x y z
                let frame = camCoords (spn p) (Vector.mkVector 0.00034 1. 0.00086)
                applyOrthoFrame frame hem_sp 
            let spdf = 2. * Math.PI * (r**2.)
            AreaLight((sp,spn,spdf),b,m)
        | _ -> failwith "Match of baseshape didn't work"
        

    let transformLight (l:ILight) (trans:Transformation) =      
        { new ILight with
            member this.lc p n sc sp p' =
                let p' = transformP trans.InvTransform p
                l.lc p n sc sp p'
            member this.ld p n sp = 
                l.ld p n sp
            member this.ls p n sc sp = 
                l.ls p n sc sp
            member this.lg p sp = 
                l.lg p sp
            member this.lpdf p n sp =
                l.lpdf p n sp
            member this.SP p = 
                let p' = transformP trans.InvTransform p
                transformP trans.Transform (l.SP p')
            member this.isShape = true    
            member this.getShape = transform (l.getShape) trans  
        }
