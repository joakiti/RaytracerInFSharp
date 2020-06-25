namespace Transformations

module Transformation = 

    open System
    open Util
    open Matrix
    open Point

    type tMatrix = Matrix

    type Transformation(transform : tMatrix, invTransform : tMatrix) =
        member this.Transform = transform
        member this.InvTransform = invTransform

    let mkIdentityTransform = new Transformation(mkIdentityMatrix, mkIdentityMatrix)

    let translate (x : float, y : float, z : float) = new Transformation(translate (x,y,z), translateInv (x,y,z))
    let rotateX (angle : float) : Transformation = new Transformation(rotateX angle, rotateXInv angle)
    let rotateY (angle : float) : Transformation = new Transformation(rotateY angle, rotateYInv angle)
    let rotateZ (angle : float) : Transformation = new Transformation(rotateZ angle, rotateZInv angle)
    let sheareXY (distance : float) : Transformation = new Transformation(sheareXY distance, invSheareXY distance)
    let sheareXZ (distance : float) : Transformation = new Transformation(sheareXZ distance, invSheareXZ distance)
    let sheareYX (distance : float) : Transformation = new Transformation(sheareYX distance, invSheareYX distance)
    let sheareYZ (distance : float) : Transformation = new Transformation(sheareYZ distance, invSheareYZ distance)
    let sheareZX (distance : float) : Transformation = new Transformation(sheareZX distance, invSheareZX distance)
    let sheareZY (distance : float) : Transformation = new Transformation(sheareZY distance, invSheareZY distance)
    let scale (x : float) (y : float) (z : float) : Transformation = new Transformation(scale x y z, scaleInv x y z)
    let mirrorX : Transformation = new Transformation(mirrorX, mirrorX)
    let mirrorY : Transformation = new Transformation(mirrorY, mirrorY)
    let mirrorZ : Transformation = new Transformation(mirrorZ, mirrorZ)
    let transpose (trans : Transformation) : Transformation = new Transformation(transpose trans.Transform,transpose trans.InvTransform)
    
    let transformP (trans : Matrix) (p : Point) : Point = trans * p
    let transformV (trans : Matrix) (v : Vector) : Vector = trans * v

    let orthRotate (u:Vector,v:Vector,m:Vector) : Transformation = new Transformation(orthMatrix (u,v,m), invOrthMatrix (orthMatrix (u,v,m)))
    
    let mergeTransformationsInverse (ts : Transformation list) : Matrix = List.foldBack ( fun (t:Transformation) s -> t.InvTransform * s) ts mkIdentityMatrix

    let mergeTransformations (ts : Transformation list) : Transformation = new Transformation((List.fold ( fun s (t:Transformation) -> t.Transform*s ) mkIdentityMatrix ts), mergeTransformationsInverse ts)
    
    
    //let transform (sh : Shape) (tr : Transformation) :  Shape = TransformedShape(sh,tr)
//let transformLight (l : light) (t : Transformation) : light = failwith "transformLight not implemented" //wut?ted" //wut?