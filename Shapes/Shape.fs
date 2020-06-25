namespace Shapes

module Shape =
    
    open Util
    open Point
    open Vector
    open Ray
    open Types
    open Transformations
    open Transformation
    open BoundingBox
    open Matrix
    open Util.Point

    type Shape(baseShape:IBaseShape, texture:ITexture) =
        member this.getMaterial = texture.getMaterial 0. 0.
        interface IShape with
            member this.boundingBox = baseShape.boundingBox
            member this.isInside p = baseShape.isInside p
            member this.shadowHitFunc ray = baseShape.shadowHitFunc ray
            member this.hitFunction ray =
                let hit = baseShape.hitFunction ray           
                match hit with
                |None -> None
                |Some(t, n) -> Some(t, n, this.getMaterial)

    let transform (sh : IShape) (tr : Transformation) = 
        let bb =
                let bb' = sh.boundingBox
                match bb' with
                | None -> None
                | Some b -> 
                    let (minx, miny, minz) = getCoord b.L
                    let (maxx, maxy, maxz) = getCoord b.H
                    let p1 = transformP tr.Transform (mkPoint minx miny maxz)
                    let p2 = transformP tr.Transform (mkPoint minx maxy maxz)
                    let p3 = transformP tr.Transform (mkPoint maxx miny maxz)
                    let p4 = transformP tr.Transform (mkPoint maxx maxy minz)
                    let p5 = transformP tr.Transform (mkPoint maxx miny minz)
                    let p6 = transformP tr.Transform (mkPoint minx maxy minz)
                    let p7 = transformP tr.Transform b.L 
                    let p8 = transformP tr.Transform b.H
                    let ps = [p1;p2;p3;p4;p5;p6;p7;p8;]
                    let (minx', miny', minz', maxx', maxy', maxz') = 
                        List.fold(fun (x1, y1, z1, x2, y2, z2) p -> 
                        let (x', y', z') = getCoord p
                        (min x1 x', min y1 y', min z1 z', max x2 x', max y2 y', max z2 z')
                        ) (infinity, infinity, infinity, -infinity, -infinity, -infinity) ps
                    mkBoundingBox (mkPoint minx' miny' minz')  (mkPoint maxx' maxy' maxz') |> Some
        let transposed = transpose tr.InvTransform
        { new IShape with
            member this.boundingBox = bb
            member this.isInside p =
                sh.isInside (transformP tr.InvTransform p)
            member this.shadowHitFunc ray = 
                let (o, d) = getComponents ray
                let o' = transformP tr.InvTransform o
                let d' = transformV tr.InvTransform d
                let ray' = mkRay o' d'
                sh.shadowHitFunc ray'
            member this.hitFunction ray = 
                let (o, d) = getComponents ray
                let o' = transformP tr.InvTransform o
                let d' = transformV tr.InvTransform d
                let ray' = mkRay o' d'
                let hit = sh.hitFunction ray'
                match hit with
                |Some(t,n',m) ->
                    let n = normalise (transformV transposed n')
                    Some(t,n,m)
                |None -> None
                }
