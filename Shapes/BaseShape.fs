namespace Shapes

module BaseShape =
    open Util.Types

    type BaseShape(shadowHitFunction, hitFunction, boundingBox) =
        interface IBaseShape with
            member this.getUV p = failwith "can this be implemented?"
            member this.shadowHitFunc ray = shadowHitFunction
            member this.hitFunction ray = hitFunction ray
            member this.boundingBox = boundingBox
            member this.isInside p = failwith "not implemented"