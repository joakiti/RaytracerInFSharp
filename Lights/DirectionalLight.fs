namespace Lights
open Scene

module DirectionalLight =

    open Util
    open System
    open Ray
    open Colour
    open Types
    open Point

    // DirectionalLight has a direction that is a vector. This vector is used to create a shadowray
    // and fire that shadowray into the scene to check wheather a given point p is coloured by the light.
    type DirectionalLight(direction:Vector, colour:Colour, intensity:float) =

        interface ILight with
            member this.lc p n sc sp p' = intensity * colour
            member this.ld p n sp = Vector.normalise(direction)
            member this.ls p n sc sp =  
                             let shadowRay = mkRay p ((this :> ILight).ld p n sp)
                             match sc.shadowHit shadowRay with
                             | Some x -> true
                             | None -> false 
            member this.lg p sp = 1.
            member this.lpdf p n sp = 1.
            member this.SP p = mkPoint 0. 0. 0.
            member this.isShape = false
            member this.getShape = failwith "Directionallight doesn't have a shape"
