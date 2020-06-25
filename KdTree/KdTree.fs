namespace KdTree

module KdTree = 

    open System
    open Util
    open Point
    open Ray
    open Colour
    open Types
    open BoundingBox
    
    (* HELPERS METHODS *)                               // Depending on the split axis, we want to use either getX, getY or getZ from Point
    let pf = [getX ; getY ; getZ]                       // pf = point-function. 
    let vf = [Vector.getX ; Vector.getY ; Vector.getZ]  // vf = vector-function.
    let bboxLength (box:IBoundingBox) = function 
    | 0 -> box.width
    | 1 -> box.height
    | _ -> box.depth
   
    (* DEFINE METHOD TO CALCULATE THE BEST SPLIT AXIS *)
    let splitList (ss:array<'a>) (ax:int) (bBox:IBoundingBox) (bboxf:'a->IBoundingBox) (minEmptySpace:float) (maxOverlap:int) =
        let getLV s = pf.[ax] (bboxf s).L       // get the value of bbox.L on this axis
        let getHV s = pf.[ax] (bboxf s).H       // get the value of bbox.H on this axis
        
        (* CHECK IF WE CAN CUT OFF EMPTY SPACE *)                                             
        let minEdgeV = getLV <| Array.minBy (fun (s) -> (getLV s)) ss
        if ((minEdgeV - pf.[ax] bBox.L) / bboxLength bBox ax) > minEmptySpace then Some([||], ss, minEdgeV, ax) else
            
        let maxEdgeV = getHV <| Array.maxBy (fun s -> (getHV s)) ss
        if ((pf.[ax] bBox.H - maxEdgeV) / bboxLength bBox ax) > minEmptySpace then Some(ss, [||], maxEdgeV, ax) else
        
        // FIND THE MIDDLE SPLIT VALUE       (to get approximately equal amount of shapes on each side)
        let splitValue =  ( Array.fold(fun ac s -> ac + getHV s + getLV s ) 0. ss ) / float (ss.Length*2)
        
        // DIVIDE THE LIST 
        let (lss,rss,c) = Array.fold(fun (lb, rb, c) s -> 
                           let l,h = (getLV s, getHV s)
                           match (l,h) with
                           | (l',h') when l' < splitValue && h' > splitValue  -> (s::lb, s::rb, (c+1))  // the shape goes into both lists, count +1
                           | (l',_) when l' < splitValue                      -> (s::lb, rb, c)         // the shape goes into the left list
                           | _                                                -> (lb, s::rb, c)         // the shape goes into the right list
                           ) ([],[], 0) ss
           
        // IF FEW SHAPES ARE ON BOTH SIDES, RETURN THE SPLITVALUE, SPLITAXIS AND LISTS OF SHAPES
        let maxOverlappingShapes = ((ss.Length * maxOverlap) / 100)
        if c > maxOverlappingShapes || rss.Length = ss.Length || lss.Length = ss.Length
        then None 
        else Some(List.toArray lss, List.toArray rss, splitValue, ax) 
        
                           
    (* METHOD CREATES A KDTREE WITH A ROOT AND A BOUNDONG BOX *)
    let mkKdTree (cutoff:int) (minEmptySpace:int->float) (maxOverlap:int) (ss:array<'a>) (bboxf:('a->IBoundingBox))  : KdTree<'a> =
        let mkBboxOfScene : IBoundingBox =        (* METHOD TO CALCULATE THE BOUNDING BOX SURROUNDING THE SCENE *)
            let startH, startL = (bboxf ss.[0]).L, (bboxf ss.[0]).H
            let ssRest = Array.skip 1 ss            // to create the tail of the list, I skip the 'head' (by skipping 1 element)
            
            let newPoint(p, q, f) =                 // given a function (f = min- or max-function) and two points, creates the new min/max point
                let px, py, pz = getCoord p 
                let qx, qy, qz = getCoord q
                mkPoint (f px qx) (f py qy) (f pz qz)
                                                        
            let (l, h) = Array.fold (fun (l,h) s -> // create l=min and h=max points of the scene (use 'head-shape' as start-state, and fold over the rest)         
                            let newL = (newPoint(l, (bboxf s).L, min))
                            let newH = (newPoint(h, (bboxf s).H, max))
                            (newL,newH)
                            ) (startH,startL) ssRest
                          
            BoundingBox(l, h) :> IBoundingBox
            
        (* METHOD THAT SORTS THE AXES OF A BOUNDING BOX FROM LONGEST TO SMALLEST *)        
        let axOrder (bBox:IBoundingBox) =  
            let axes = [|(bBox.width, 0); (bBox.height, 1); (bBox.depth, 2)|]
            Array.sortByDescending (fun (v,a) -> v) axes |> Array.map (fun (v,a) -> a) 
        
        (* METHOD TO DIVIDE THE BOUNDING INTO TWO: LEFT AND RIGHT BOX*)
        let splitBox (box:IBoundingBox) ax sv = 
            let lx, ly, lz = getCoord box.L
            let hx, hy, hz = getCoord box.H
            match ax with    
            | 0 ->  (mkBoundingBox box.L (mkPoint sv hy hz), mkBoundingBox (mkPoint sv ly lz) box.H) // if we split on x, make v the new x
            | 1 ->  (mkBoundingBox box.L (mkPoint hx sv hz), mkBoundingBox (mkPoint lx sv lz) box.H) // if we split on y, make v the new y
            | _ ->  (mkBoundingBox box.L (mkPoint hx hy sv), mkBoundingBox (mkPoint lx ly sv) box.H) // ...
            

        (* METHOD TO RECURSIVELY CONSTRUCT A TREE*)
        let rec mkTreeHelper (ss:array<'a>) (bBox:IBoundingBox) (depth:int) : Tree<'a> = 
            if (ss.Length <= cutoff) || depth > 9999 then Leaf(ss) else     // cutoff
            
            (* recursively try to split on the axes *)
            let axes = axOrder bBox     // sort the axes based on the lengths of the bbox - biggest one first
            let rec splitOnAxes n =     // recursively try to find a good split
                if n > 2 then None
                else 
                    let split = splitList ss axes.[n] bBox bboxf (minEmptySpace depth) maxOverlap   // try to split on this axis 
                    match split with
                    | None            -> splitOnAxes (n+1)                   // try next axis
                    | _               -> split                               // return good split
            
            (* get the split and create the node *)
            let split = splitOnAxes 0
            match split with 
            | None -> Leaf(ss)
            | Some(l, r, sv, sa) -> 
                let (lb, rb) = splitBox bBox sa sv              // divide the boundingboxes into left and right box
                let lc = mkTreeHelper (l) (lb) (depth+1)        // rec. make left child
                let rc = mkTreeHelper (r) (rb) (depth+1)        // rec. make right child
                Node (lc, sv, sa, rc)                           // return 'root' node 
        
        (* CALC THE SCENE AND MAKE THE TREE*)
        let sceneBbox = mkBboxOfScene
        let tree = mkTreeHelper ss sceneBbox 0
        Kdt(tree, sceneBbox)                        // return a KdTree using MkTreeHelper and calcSceneBox
    

    let mkKdTree2 (ss:array<'a>) (bboxf:('a->IBoundingBox)) = 
        let cutoff = 1
        let minEmptySpace = fun d -> if d >= 4 then 0.30 else 0.1 + (0.05 * float d)  
        let maxOverlap = 50                           
        mkKdTree cutoff minEmptySpace maxOverlap ss bboxf
    
    (* METHOD TO CHECK IF RAY HIT A SHAPE IN THE TREE *)  
    let traverse (Kdt(root,scene)) (r:Ray) (bboxf:'a->IBoundingBox) (hitf:'a->Ray->option<'b>) (distf:'b->float) : option<'b> = 
        
        (* METHOD THAT, GIVEN A LIST OF SHAPES, RETURNS THE ONE CLOSEST TO R.ORIGIN *)
        let closestHit shapes =  
            Array.fold (fun bState s -> 
                    let bShape = hitf s r                           // check with the hitfunction
                    match (bState, bShape) with                     // check if shape.hit is closer than hitState
                    | (None, None)                    -> None       // no hit found yet
                    | (None, _)                       -> bShape        // if hitState=None, then any first hit is closer
                    | (_, None)                       -> bState     // if hit=None then hitState is closer
                    | (Some b, Some b')   -> if distf b' < distf b  // if both are hits, return the closest
                                                then bShape 
                                                else bState  
                ) None shapes   // (start-state = None)
        
   
        (* METHOD TO RECURSIVLY SEARCH A TREE *)
        let rec search ((node:Tree<'a>), (r:Ray), (t:float), (t':float)) : option<'b> =   
            match node with 
            | Leaf(ss)  ->                            (* THIS IS THE SEARCH-LEAF(node, r, t) *)                   
                let hit = closestHit ss                 // find the closest shape in the leaf
                match hit with                          
                | Some b when distf b < t' -> hit       // if distanceToHit < distanceToExitScene (avoids black spots)
                | _                        -> None
            | Node(lc,split,ax,rc)  -> 
                let dv = getDirection r |> vf.[ax]      // value of either d.x, d.y or d.z
                let ov = getOrigin r |> pf.[ax]         // value of either o.x, o.y, o.z
        
                if dv = 0.0 then                         // if r is orthogonal to split-axis, simply search either lc or rc:
                    if ov <= split then                     // origin <= split ? search left : search right
                        search (lc, r, t, t') 
                    else            
                        search (rc, r, t, t')            
                else                                    // if r is not orthogonal to split-axis
                    let fst, snd = if (dv > 0.0) then lc, rc else rc, lc // if r goes 'normal' direction, it hit left child first ..
                    let tHit = (split - ov) / dv        // r's distance to split-value (tb)
                    if tHit <= t || tHit <= 0.0 then 
                        search(snd, r, t, t')
                    else 
                        if tHit >= t' then 
                            search(fst, r, t, t')
                        else
                            match search(fst, r, t, tHit) with
                            | Some b    -> Some b
                            | _         -> search(snd, r, tHit, t')                                                             
        match scene.Intersects r with                       // Only if the ray intersects the scene of the tree, we search it 
        | Some(t, t')   -> search (root, r, t, t')         // t=distToEntrance, t'=distToExit
        | None          ->  None

