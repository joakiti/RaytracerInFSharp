namespace Shapes

open Util
open Util.Types
open Util.Ray
open Util.BoundingBox

module CSG = 
    
    open System
    
    // the small distance the ray origin and the hits are moved.
    let eps = 1e-6
    
    // creates a new origin for the ray to be refired, by moving the current origin by the length to the 
    // hitpoint. To move it a bit outside the surface the eps value is added to the origin of the new ray.
    let refireRay p ray d = 
                mkRay (Point.move (getOrigin ray) ((d+eps) * (getDirection ray))) (getDirection ray)   
    
    // adds two boundingboxes together by taking two boundingboxes and extracting their L and H and
    // compare the coordinates. The new L is the MIN for all coordinates and the H is the MAX for all 
    // coordinates.
    let boundingBoxAddition (b1:IBoundingBox option) (b2:IBoundingBox option) =
                match b1, b2 with
                | Some b1', Some b2' -> 
                    let (lx1,ly1,lz1) = Point.getCoord b1'.L
                    let (lx2,ly2,lz2) = Point.getCoord b2'.L
                    // finding L for the new boundingBox
                    let L = Point.mkPoint (Math.Min (lx1,lx2)) (Math.Min (ly1,ly2)) (Math.Min (lz1,lz2))
                    
                    let (hx1,hy1,hz1) = Point.getCoord b1'.H                    
                    let (hx2,hy2,hz2) = Point.getCoord b2'.H 
                    // finding H for the new boundingBox
                    let H = Point.mkPoint (Math.Max (hx1,hx2)) (Math.Max (hy1,hy2)) (Math.Max (hz1,hz2))
                                  
                    Some (mkBoundingBox L H)
                | _ -> None 
                
    // creates intersection of two boundingboxes together by taking two boundingboxes and extracting 
    // their L and H and compare the coordinates. The new L is the MAX for all coordinates and the H is  
    // the MIN for all coordinates.
    let boundingBoxIntersection (b1:IBoundingBox option) (b2:IBoundingBox option) =
            match b1, b2 with
            | Some b1', Some b2' -> 
                let (lx1,ly1,lz1) = Point.getCoord b1'.L
                let (lx2,ly2,lz2) = Point.getCoord b2'.L
                // finding L for the new boundingBox
                let L = Point.mkPoint (Math.Max (lx1,lx2)) (Math.Max (ly1,ly2)) (Math.Max (lz1,lz2))
                
                let (hx1,hy1,hz1) = Point.getCoord b1'.H                    
                let (hx2,hy2,hz2) = Point.getCoord b2'.H 
                // finding H for the new boundingBox
                let H = Point.mkPoint (Math.Min (hx1,hx2)) (Math.Min (hy1,hy2)) (Math.Min (hz1,hz2))
                
                Some (mkBoundingBox L H)
            | _ -> None 
    
    // Group fires a ray at both shapes and returns the closest hitpoint. 
    // doesn't remove internal edges compared to union.
    let group (s1:IShape) (s2:IShape) : IShape =
          // Checks that the shapes s1 and s2 are solid
          let insideS1 =
              match s1.isInside with
              | i -> i
              
          let insideS2 = 
              match s2.isInside with
              | i -> i
                                       
          // The inside function for the new shape, checks if p is inside either of the shapes.
          let inside p = (insideS1 p) || (insideS2 p)
          
          let boundingBox = boundingBoxAddition (s1.boundingBox) (s2.boundingBox)
          
          // New hitfunction made from the hitfunctions of s1 and s2       
          let hf1 = s1.hitFunction
          let hf2 = s2.hitFunction 
                        
          let hitFunc ray =
              let calcHitFunc ray =
                  match (hf1 ray, hf2 ray) with
                  | (Some(t1,n1,m1), Some(t2,n2,m2)) -> 
                                               if t2 < t1 then Some(t2,n2,m2)
                                               else Some(t1,n1,m1)
                  | (Some(t1,n1,m1), None) -> Some(t1,n1,m1)
                  | (None, Some(t2,n2,m2)) -> Some(t2,n2,m2)
                  | _ -> None
              calcHitFunc ray

          // New shadowHitfunction made from the shadowHitfunctions of s1 and s2       
          let shf1 = s1.shadowHitFunc
          let shf2 = s2.shadowHitFunc
              
          let shadowHitFunc ray = 
              let calcHitFunc ray =
                  match (shf1 ray, shf2 ray) with
                  | (Some(t1), Some(t2)) -> 
                                           if t2 < t1 then Some(t2)
                                           else Some(t1)
                  | (Some(t1), None) -> Some(t1)
                  | (None, Some(t2)) -> Some(t2)
                  | _ -> None
              calcHitFunc ray
                 
          { new IShape with
                  member this.boundingBox = boundingBox
                  member this.isInside p = inside p
                  member this.shadowHitFunc ray = shadowHitFunc ray
                  member this.hitFunction ray = hitFunc ray                          
          }                  
    
    // Union fires a ray at both shapes. Checks what shape it hits first, by comparing the distances "t"
    // from the hits. It checks if the shape with the closest hit is inside the other shape. If it is
    // inside, the ray is refired. This is repeated until a hit is obtained that is not inside the other shape. 
    let union (s1:IShape) (s2:IShape) : IShape =  
    
           // Checks that the shapes s1 and s2 are solid
           let insideS1 =
               match s1.isInside with
               | i -> i
               
           let insideS2 = 
               match s2.isInside with
               | i -> i
                                   
           // The boundingbox for the new shape
           let boundingBox = boundingBoxAddition (s1.boundingBox) (s2.boundingBox)
           
           // The inside function for the new shape, checks if p is inside either of the shapes.
           let inside p = (insideS1 p) || (insideS2 p)
                      
           // New hitfunction made from the hitfunctions of s1 and s2       
           let hf1 = s1.hitFunction
           let hf2 = s2.hitFunction                      
           
           let hitFunc ray = 
               let rec calcHitFunc ray =
                   let rayo = getOrigin ray 
                   let rayDir = getDirection ray
                   match (hf1 ray, hf2 ray) with
                   | (Some(t1,n1,m1), Some(t2,n2,m2)) -> 
                            let hp1 = getPointOnRay t1 ray
                            let hp2 = getPointOnRay t2 ray
                            let hit = 
                                if t2 < t1 then 
                                    if (inside rayo) then 
                                        if (insideS1 hp2) then                                        
                                            match calcHitFunc (refireRay hp2 ray t2) with
                                            // the distance is added up recursively.
                                            | Some (t2',n2,m2) -> Some (t2'+t2+eps,n2,m2)
                                            | None -> None                                             
                                        else Some(t2+eps,n2,m2)
                                    else Some(t2+eps,n2,m2)
                                else 
                                    if (inside rayo) then 
                                        if (insideS2 hp1) then
                                            match calcHitFunc (refireRay hp1 ray t1) with
                                            | Some (t1',n1,m1) -> Some (t1'+t1+eps,n1,m1)
                                            | None -> None                                             
                                        else Some(t1+eps,n1,m1)
                                    else Some(t1+eps,n1,m1)
                            hit
                   | (Some(t1,n1,m1), None) -> Some(t1+eps,n1,m1)
                   | (None, Some(t2,n2,m2)) -> Some(t2+eps,n2,m2)
                   | _ -> None
               calcHitFunc ray 
               
           // New shadowHitfunction made from the shadowHitfunctions of s1 and s2       
           let shf1 = s1.shadowHitFunc
           let shf2 = s2.shadowHitFunc                      
           
           let shadowHitFunc ray = 
               let rec calcHitFunc ray =
                   let rayo = getOrigin ray 
                   let rayDir = getDirection ray
                   match (shf1 ray, shf2 ray) with
                   | (Some(t1), Some(t2)) -> 
                            let hp1 = getPointOnRay t1 ray
                            let hp2 = getPointOnRay t2 ray
                            let hit = 
                                if t2 < t1 then 
                                    if (inside rayo) then 
                                        if (insideS1 hp2) then                                        
                                            match calcHitFunc (refireRay hp2 ray t2) with
                                            | Some (t2') -> Some (t2'+t2+eps)
                                            | None -> None                                             
                                        else Some(t2+eps)
                                    else Some(t2+eps)
                                else 
                                    if (inside rayo) then 
                                        if (insideS2 hp1) then
                                            match calcHitFunc (refireRay hp1 ray t1) with
                                            | Some (t1') -> Some (t1'+t1+eps)
                                            | None -> None                                             
                                        else Some(t1+eps)
                                    else Some(t1+eps)
                            hit
                   | (Some(t1), None) -> Some(t1+eps)
                   | (None, Some(t2)) -> Some(t2+eps)
                   | _ -> None
               calcHitFunc ray 
            
            //make new shape        
           { new IShape with
                  member this.boundingBox = boundingBox
                  member this.isInside p = inside p
                  member this.hitFunction ray = hitFunc ray
                  member this.shadowHitFunc ray = shadowHitFunc ray                          
           }           

    // A ray is fired at both shapes, the distances of the hits are compared. The closest hit 
    // that is inside the other shape is returned. If eg. the hit on s1 is closest, but the point is 
    // not inside s2, the ray is refired. 
    let intersection (s1:IShape) (s2:IShape) : IShape = 
       
           // Checks that the shapes s1 and s2 are solid
           let insideS1 =
               match s1.isInside with
               | i -> i
               
           let insideS2 = 
               match s2.isInside with
               | i -> i
                                   
           // The boundingbox for the new shape
           let boundingBox = boundingBoxIntersection (s1.boundingBox) (s2.boundingBox)            
           
           // The inside function for the new shape, checks if p is inside either of the shapes.
           let inside p = (insideS1 p) && (insideS2 p)           
           
           // New hitfunction made from the hitfunctions of s1 and s2       
           let hf1 = s1.hitFunction
           let hf2 = s2.hitFunction
           
           let hitFunc ray = 
                let rec calcHitFunc ray =
                     let rayDir = getDirection ray 
                     match (hf1 ray, hf2 ray) with
                     | (Some(t1,n1,m1), Some(t2,n2,m2)) -> 
                              let hp1 = getPointOnRay t1 ray
                              let hp2 = getPointOnRay t2 ray
                              if (t2 <= t1) then                                                             
                                  if (insideS1 hp2) then Some(t2,n2,m2)
                                  else                                                                            
                                      match calcHitFunc (refireRay hp2 ray t2) with
                                      | Some (t2',n2,m2) -> Some (t2'+t2+eps,n2,m2)
                                      | None -> None    
                              else 
                                  if (insideS2 hp1) then Some(t1,n1,m1)
                                  else                                       
                                      match calcHitFunc (refireRay hp1 ray t1) with
                                      | Some (t1',n1,m1) -> Some (t1'+t1+eps,n1,m1)
                                      | None -> None                                                     
                     | _ -> None
                calcHitFunc ray     

           // New shadowHitfunction made from the shadowHitfunctions of s1 and s2       
           let shf1 = s1.shadowHitFunc
           let shf2 = s2.shadowHitFunc
           
           let shadowHitFunc ray = 
                let rec calcHitFunc ray =
                     let rayDir = getDirection ray 
                     match (shf1 ray, shf2 ray) with
                     | (Some(t1), Some(t2)) -> 
                              let hp1 = getPointOnRay t1 ray
                              let hp2 = getPointOnRay t2 ray
                              if (t2 <= t1) then                                                             
                                  if (insideS1 hp2) then Some(t2)
                                  else                                                                            
                                      match calcHitFunc (refireRay hp2 ray t2) with
                                      | Some (t2') -> Some (t2'+t2+eps)
                                      | None -> None    
                              else 
                                  if (insideS2 hp1) then Some(t1)
                                  else                                       
                                      match calcHitFunc (refireRay hp1 ray t1) with
                                      | Some (t1') -> Some (t1'+t1+eps)
                                      | None -> None                                                     
                     | _ -> None
                calcHitFunc ray                                                          
                      
           //make new shape                    
           { new IShape with
                    member this.boundingBox = boundingBox
                    member this.isInside p = inside p
                    member this.hitFunction ray = hitFunc ray   
                    member this.shadowHitFunc ray = shadowHitFunc ray                       
           }
                    
    // Subtraction checks if the closest hit is on s1 is inside s2, if it is inside the ray is refired, if not
    // the hit is returned. If the closest hit is on s2 and the point is inside s1, the hit is returned else
    // the ray is refired.
    let subtraction (s1:IShape) (s2:IShape) : IShape =

           // Checks that the shapes s1 and s2 are solid
           let insideS1 =
               match s1.isInside with
               | i -> i
               
           let insideS2 = 
               match s2.isInside with
               | i -> i
                                   
           // The boundingbox for the new shape
           let boundingBox = s1.boundingBox 
           
           // The inside function for the new shape, checks if p is inside either of the shapes.
           let inside p = (insideS1 p) && (not (insideS2 p))
              
           // New hitfunction made from the hitfunctions of s1 and s2       
           let hf1 = s1.hitFunction
           let hf2 = s2.hitFunction
           
           let hitFunc ray = 
                 let rec calcHitFunc ray =
                     let ro = getOrigin ray
                     let rayDir = getDirection ray
                     match (hf1 ray, hf2 ray) with
                     | (Some(t1,n1,m1), Some(t2,n2,m2)) -> 
                                   let hp1 = getPointOnRay t1 ray
                                   let hp2 = getPointOnRay t2 ray
                                   if (t2 < t1) then
                                         if(insideS1 hp2) then Some (t2+eps, n2, m2) 
                                         else
                                            match calcHitFunc (refireRay hp2 ray t2) with
                                            | Some (t2',n2,m2) -> Some (t2'+t2+eps,n2,m2)
                                            | None -> None                                        
                                      else
                                         if(insideS2 hp1) then
                                            match calcHitFunc (refireRay hp1 ray t1) with
                                            | Some (t1',n1,m1) -> Some (t1'+t1+eps,n1,m1)
                                            | None -> None  
                                         else Some (t1+eps,n1,m1) 
                     | (Some(t1,n1,m1),None) -> Some (t1+eps,n1,m1)
                     | _ -> None
                 calcHitFunc ray

           // New hitfunction made from the hitfunctions of s1 and s2       
           let shf1 = s1.shadowHitFunc
           let shf2 = s2.shadowHitFunc
           
           let shadowHitFunc ray = 
                 let rec calcHitFunc ray =
                     let ro = getOrigin ray
                     let rayDir = getDirection ray
                     match (shf1 ray, shf2 ray) with
                     | (Some(t1), Some(t2)) -> 
                                   let hp1 = getPointOnRay t1 ray
                                   let hp2 = getPointOnRay t2 ray
                                   if (t2 < t1) then
                                         if(insideS1 hp2) then Some (t2+eps) 
                                         else
                                            match calcHitFunc (refireRay hp2 ray t2) with
                                            | Some (t2') -> Some (t2'+t2+eps)
                                            | None -> None                                        
                                      else
                                         if(insideS2 hp1) then
                                            match calcHitFunc (refireRay hp1 ray t1) with
                                            | Some (t1') -> Some (t1'+t1+eps)
                                            | None -> None  
                                         else Some (t1+eps) 
                     | (Some(t1),None) -> Some (t1+eps)
                     | _ -> None
                 calcHitFunc ray
                 
                                  
            //make new shape        
           { new IShape with
                 member this.boundingBox = boundingBox
                 member this.isInside p = inside p
                 member this.hitFunction ray = hitFunc ray
                 member this.shadowHitFunc ray = shadowHitFunc ray                          
           }
    

    
        
        
        
        
        
                
        
                   
        
                           
                   