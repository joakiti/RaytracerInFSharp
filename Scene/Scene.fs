namespace Scene

module Scene =
    
    open System
    open Util
    open Point
    open Vector
    open KdTree.KdTree
    open Types
    
    let nearestShadowHit (shapes:IShape array) ray hit = 
        Array.fold (fun minhit (sh:IShape) ->
                        match (minhit, sh.shadowHitFunc ray) with
                        | (None, hit) -> hit
                        | (hit, None) -> hit
                        | Some(mdis) as mhit, Some(dis) when mdis < dis -> mhit
                        | Some(mdis), (Some(dis) as hit) when mdis >= dis -> hit
                   ) hit shapes
                   
    let nearestHit (shapes:IShape array) ray hit = 
        Array.fold (fun minhit (sh:IShape) ->
                        match (minhit, sh.hitFunction ray) with
                        | (None, hit) -> hit
                        | (hit, None) -> hit
                        | Some(mdis, _, _) as mhit, Some(dis, _, _) when mdis < dis -> mhit
                        | Some(mdis, _, _), (Some(dis, _, _) as hit) when mdis >= dis -> hit
                   ) hit shapes

    type Scene(lightShapes:KdTree<IShape> option, shapes:IShape array, kdTree:KdTree<IShape> option, lightList:ILight list, ambientLight:IAmbientLight, maxDepth:int) =
        interface IScene with
            member this.sl = shapes
            member this.lss = lightList
            member this.la = ambientLight
            member this.m = maxDepth
            member this.eps = 0.0001            
            member this.shadowHit r = 
                let kdHit = match kdTree with 
                               |None -> None
                               |Some t -> traverse t r (fun (s:IShape) -> match s.boundingBox with Some b -> b | None -> failwith "no bounding box") (fun s r -> s.shadowHitFunc r) (fun h -> h) 
                nearestShadowHit shapes r kdHit      
            
            member this.hitFunction r =
                let shadowHit = 
                    let kdHit = match kdTree with 
                                   |None -> None
                                   |Some t -> traverse t r (fun (s:IShape) -> match s.boundingBox with Some b -> b | None -> failwith "no bounding box") (fun s r -> s.hitFunction r) (fun h -> match h with (t, _, _) -> t) 
                   
                    let listHit = nearestHit shapes r kdHit
                    match (kdHit, listHit) with
                    | (None, None) -> None
                    | (Some h, None) -> Some h
                    | (None, Some h) -> Some h
                    | (Some(t1, _, _), (Some(t2, _, _) as h2)) when t1 >= t2 -> h2
                    | (Some(t1, _, _) as h1, Some(t2, _, _)) when t1 < t2 -> h1   
                    
                let lightHit = match lightShapes with 
                                   |None -> None
                                   |Some t -> traverse t r (fun (s:IShape) -> match s.boundingBox with Some b -> b | None -> failwith "no bounding box") (fun s r -> s.hitFunction r) (fun h -> match h with (t, _, _) -> t) 
                   
                match (shadowHit, lightHit) with
                | (None, None) -> None
                | (Some h, None) -> Some h
                | (None, Some h) -> Some h 
                | (Some(t1, _, _), (Some(t2, _, _) as h2)) when t1 >= t2 -> h2
                | (Some(t1, _, _) as h1, Some(t2, _, _)) when t1 < t2 -> h1

    let mkScene (s : IShape list) (l : ILight list) (a : IAmbientLight)(m : int) : Scene =
        let hasShape = List.filter (fun (l:ILight) -> l.isShape) l 
        let lightShapes = List.map (fun (l:ILight) -> l.getShape) hasShape |> Array.ofList 
        let hasBB (s:IShape) = match s.boundingBox with
                               | None -> false
                               | Some _ -> true
        let nonBB = List.filter (fun sh -> (not << hasBB) sh ) s |> Array.ofList
        let withBB = List.filter (hasBB) s
        let tree = if List.isEmpty withBB then None else mkKdTree2 (Array.ofList withBB) (fun s -> match s.boundingBox with Some b -> b | None -> failwith "no bounding box") |> Some
        let lightShapeTree = if Array.isEmpty lightShapes then None else mkKdTree2 lightShapes (fun s -> match s.boundingBox with Some b -> b | None -> failwith "no bounding box") |> Some 
        Scene(lightShapeTree, nonBB, tree, l, a, m)