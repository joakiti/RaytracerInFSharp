namespace Util

module Colour =

    exception ColourException
    type Colour =
      | RGB of float * float * float
      override rgb.ToString() =
        match rgb with
          RGB(r,g,b) -> "["+r.ToString()+","+g.ToString()+","+b.ToString()+"]"

    let mkColour r g b = if r < 0.0 || g < 0.0 || b < 0.0 
                         then raise ColourException 
                         else RGB(r,g,b)
    let getR (RGB(r,_,_)) = r
    let getG (RGB(_,g,_)) = g
    let getB (RGB(_,_,b)) = b
    let scale (RGB(r,g,b)) s =  
                        if s < 0.0
                        then raise ColourException
                        else RGB(r*s,g*s,b*s) 

    let merge w (RGB(r1,g1,b1)) (RGB(r2,g2,b2)) = 
                         if w < 0.0 || w > 1.0
                         then raise ColourException
                         else
                          let v = 1.0 - w  
                          RGB(r1*w + r2*v, g1*w + g2*v, b1*w+b2*v)

    let toColor (RGB(r,g,b)) =
                let (r,g,b) = 
                    if r > 1. || g > 1. || b > 1.
                                then 
                                    let scale = 1. / List.max([r;g;b])
                                    (scale*r,scale*g,scale*b)
                    else (r,g,b)
                System.Drawing.Color.FromArgb(min (int (sqrt r*255.0)) 255,
                                              min (int (sqrt g*255.0)) 255,
                                              min (int (sqrt b*255.0)) 255)

    let fromColor (c:System.Drawing.Color) = 
                mkColour (System.Math.Pow (float c.R / 255.0, 2.0))
                         (System.Math.Pow (float c.G / 255.0, 2.0))
                         (System.Math.Pow (float c.B / 255.0, 2.0))

    let average clrs = 
        let (tr, tg, tb) = List.fold (fun (ar, ag, ab) (RGB(r,g,b)) -> (ar+r, ag+g, ab+b)) (0., 0., 0.) clrs
        let l = List.length clrs |> float
        mkColour (tr/l) (tg/l) (tb/l)

    type Colour with
      static member ( + ) (RGB(r1,g1,b1),RGB(r2,g2,b2)) = mkColour (r1+r2) (g1+g2) (b1+b2)
      static member ( * ) (RGB(r1,g1,b1),RGB(r2,g2,b2)) = mkColour (r1*r2) (g1*g2) (b1*b2)
      static member ( * ) (s,c) = scale c s
      static member ( ^^ ) (RGB(r,g,b), f:float) = mkColour (r**f) (g**f) (b**f)