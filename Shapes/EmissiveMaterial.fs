namespace Shapes

module EmissiveMaterial =

    
    open System.Drawing
    open Util
    open Colour
    open Types

    type EmissiveMaterial(colour:Colour, intensity:float) =
        let er = intensity * colour
        let black = mkColour 0. 0. 0.
        interface IMaterial with
            member this.colour sc d p n depth = 
                if -n * d > 0.
                then er
                else black
