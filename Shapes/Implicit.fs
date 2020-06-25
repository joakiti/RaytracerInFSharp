namespace Shapes

module Implicit =

    open Util
    open Point
    open Vector
    open Implicit
    open ExprParse
    open ExprToPoly
    open ExprUtil
    open Types

    type Implicit(poly, derx, dery, derz, exp) =
        interface IBaseShape with
            member this.getUV p = (0.,0.)
            member this.boundingBox = None
            member this.shadowHitFunc ray =
                let (p, v) = Ray.getComponents ray
                let (ox,oy,oz) = Point.getCoord p
                let (dx,dy,dz) = Vector.getCoord v
                let vars = [|ox;oy;oz;dx;dy;dz|]
                let distance = evalPoly poly vars
                distance
            member this.hitFunction ray = 
                let (p, v) = Ray.getComponents ray
                let (ox,oy,oz) = Point.getCoord p
                let (dx,dy,dz) = Vector.getCoord v
                let vars = [|ox;oy;oz;dx;dy;dz|]
                
                let distance = evalPoly poly vars
                match distance with 
                | None -> None
                | Some t -> 
                    let p' = p + t * v
                    let (x, y, z) = Point.getCoord p'
                    let vars = [|x;y;z|]
                    let nx = evalExpr vars derx 
                    let ny = evalExpr vars dery 
                    let nz = evalExpr vars derz
                    let normal = mkVector nx ny nz |> normalise
                    Some(t, normal)

            member this.isInside p =
                    let (x, y, z) = Point.getCoord p
                    let vars = [|x;y;z|]
                    match evalExpr vars exp with
                    | c when c < 0. -> true
                    | _ -> false
    
    let mkImplicit s = 
        let exp = parseStr s
        let dx = derive exp "x"
        let dy = derive exp "y"
        let dz = derive exp "z"
        let (P(poly)) = exprToPoly exp
        let l = Map.fold(fun acc _ v -> v::acc) [] poly
        Implicit(l, dx, dy, dz, exp)

