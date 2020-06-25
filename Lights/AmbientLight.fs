namespace Lights

module AmbientLight =
    
    open Util
    open Types
    open System
    open Colour
                
    // Has an intensity and a colour. The colour of light is calculated by multiplying the two of them.
    type AmbientLight(colour:Colour, intensity:float) =
       
        interface IAmbientLight with
            member this.lc p v sc = intensity * colour 
    
