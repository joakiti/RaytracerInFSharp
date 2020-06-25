namespace Shapes

module TriangleMesh =
    open Util.Types
    open Triangle
    open KdTree

    let getBox = (fun (s:IBaseShape) -> match s.boundingBox with | Some b -> b | None -> failwith "no bounding box")

    type TriangleMesh(BaseTriangles : KdTree<IBaseShape> , boundingBox) =
        let shapes = BaseTriangles
        interface IBaseShape with
            member this.getUV p = (0.,0.)
            member this.shadowHitFunc ray =
               KdTree.traverse shapes ray 
                    getBox
                    (fun s r -> s.shadowHitFunc r) 
                    (fun h -> h) 
            member this.hitFunction ray = 
                KdTree.traverse shapes ray 
                    getBox
                    (fun s r -> s.hitFunction r) 
                    (fun h -> match h with (t, _) -> t)                          
            member this.boundingBox = boundingBox
            member this.isInside p = failwith "isInside not implemented "

    let mkTriangleMesh(shapes: IBaseShape array, box) = 
        let kdTree = KdTree.mkKdTree2 shapes (fun s -> match s.boundingBox with Some b -> b | None -> failwith "no bounding box")
        TriangleMesh(kdTree, box)