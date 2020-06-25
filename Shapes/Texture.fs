namespace Shapes

module Texture =
    
    open Util.Types

    type Texture(texFunction) =
        interface ITexture with
            member this.getMaterial u v = 
                texFunction u v

    type MatteTexture(material) =
        interface ITexture with
            member this.getMaterial u v = 
                material