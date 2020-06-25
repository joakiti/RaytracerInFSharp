namespace Lights

module PointLight =

    open Util
    open System
    open Ray
    open Colour
    open Types
    open Point
    
    // PointLight has a point where it is placed. This point is used to create a shadowray from the hitpoint
    // spanned to the lights position point, and fire that shadowray into the scene to check wheather a 
    // given point p is coloured by the light.
    type PointLight(position:Point, colour:Colour, intensity:float) =
        member this.position = position 
        member this.colour = colour 
        member this.intensity = intensity
            
        interface ILight with
            member this.lc p n sc sp p'= intensity * colour
            member this.ld p n sp = Vector.normalise(Point.D p position)
            member this.ls p n sc sp = 
                            let dir = Point.D p position
                            let dirNorm = (this :> ILight).ld p n sp 
                            let shadowRay = mkRay p dirNorm
                            match sc.shadowHit shadowRay with
                            | Some (t) -> t < Vector.magnitude(dir)
                            | None -> false
            member this.lg p sp = 1.
            member this.lpdf p n sp = 1.
            member this.SP p = mkPoint 0. 0. 0.
            member this.isShape = false
            member this.getShape = failwith "Pointlight doesn't have a shape"
