namespace Util

module Types = 
    
    open System
    open Ray
    open Point
    open Colour
    
    type IScene =
        abstract member sl : IShape array
        abstract member lss : ILight list
        abstract member la : IAmbientLight
        abstract member m : int
        abstract member eps : float
        abstract member shadowHit : Ray -> (float) option
        abstract member hitFunction : Ray -> (float*Vector*IMaterial) option
    
    and Tree<'a> = Leaf of array<'a> | Node of leftChild:Tree<'a> * split:float * axis:int * rightChild:Tree<'a>
    
    and KdTree<'a> = Kdt of root:Tree<'a> * scene:IBoundingBox

    and ILight = 
        abstract member lc : Point -> Vector -> IScene -> Point -> Point -> Colour 
        abstract member ld : Point -> Vector -> Point -> Vector
        abstract member ls : Point -> Vector -> IScene -> Point -> bool
        abstract member lg : Point -> Point -> float
        abstract member lpdf : Point -> Vector -> Point -> float 
        abstract member SP : Point -> Point          
        abstract member isShape : bool
        abstract member getShape : IShape
              
        
    and IAmbientLight = 
        // lc takes a Point and a shape list only because ambient occlusion needs those parameters.
        // lc also takes a Vector that is the normal of the point  
        abstract member lc : Point -> Vector -> IScene -> Colour 
   
    and IBoundingBox =
        abstract member L : Point 
        abstract member H : Point
        abstract member Intersects: Ray -> option<float * float>
        abstract member width : float
        abstract member height : float
        abstract member depth : float
    
    and IMaterial =
        abstract member colour : sc:IScene -> rayDirection:Vector -> hitPoint:Point -> normal:Vector -> depth:int -> Colour
             
    and ITexture =
        abstract getMaterial : u:float -> v:float -> IMaterial

    and IBaseShape = 
        abstract member boundingBox : IBoundingBox option
        abstract member getUV : Point -> float * float
        abstract member hitFunction : Ray -> option<float * Vector>
        abstract member isInside : Point -> Boolean
        abstract member shadowHitFunc : Ray -> option<float>

    and IShape = 
        abstract member boundingBox : IBoundingBox option
        abstract member isInside : Point -> Boolean
        abstract member hitFunction : Ray -> option<float * Vector * IMaterial>
        abstract member shadowHitFunc : Ray -> option<float>


        
