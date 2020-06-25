namespace Util 

open Vector 
open Point

module Orthonormal =

    //calculates the orthonormal coordinate space
    let camCoords a b = 
                let w = normalise a
                let v = normalise (b % w)
                let u = w % v
                (u,v,w)
                                                                
    // applies an orthonormal frame to a sample point at returns the point
    let applyOrthoFrame frame sp = // translate point into world coordinates returns point. 
                let ((u:Vector),(v:Vector),(w:Vector)) = frame
                let (spx,spy,spz) = Point.getCoord sp
                let v = spx*u+spy*v+spz*w |> normalise
                let (vx,vy,vz) = Vector.getCoord v
                mkPoint vx vy vz
                
    // applies an orthonormal frame to a sample point at returns the vector. 
    let applyOrthoFrameToVec frame sp = // translate point into world coordinates returns vector. 
                    let ((u:Vector),(v:Vector),(w:Vector)) = frame
                    let (spx,spy,spz) = Point.getCoord sp
                    spx*u+spy*v+spz*w
            