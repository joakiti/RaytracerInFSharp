namespace Scene

module ThinLensCamera =

    open System.Drawing
    open Util
    open Sampling.Sampler
    open Colour
    open Ray
    open Point
    open Vector
    open Types
    
    type Camera = Camera.Camera

    type ThinLensCamera(pos : Point, lookat : Point, up : Vector, zoom : float, width : float, height : float, resX : int, resY : int, r : float, f : float, viewPlane : Sampler, lens : Sampler) =
        interface Camera with
            member this.takePicture sc =

                let baseClr = mkColour 0. 0. 0.
                
                let picture = new Bitmap(resX, resY)

                //calculate size of viewplane pixel and distance vector from camera position to lookat point
                let pixelWidth = width / float resX
                let pixelHeight = height / float resY
                let distanceVector = D lookat pos

                //calculates the orthonormal coordinate space
                let camCoords a b = 
                    let w = normalise a
                    let u = normalise (b % w)
                    let v = w % u
                    (w,u,v)
                
                //calculates the coordinate space from the above distance vector and the up vector
                let (w, u, v) = camCoords distanceVector up

                //map sampler to unit hemisphere
                let lensR = mapPointsToUD lens
                
                //calculate point in a viewplane pixel
                let pointInPixel x y sx sy = mkPoint(pixelWidth * (x - (float resX/2.0)+sx)) (pixelHeight * (y - (float resY/2.0)+sy)) (-zoom) //is the y coord correct?

                //calculate direction vector and origin for ray
                let rayDirectionVector p =
                    let (qx, qy, zoom) = Point.getCoord p
                    let px = (f*qx)/(-zoom)
                    let py = (f*qy)/(-zoom)
                
                    let (a,b) = getNext lensR
                    let lx = a*r
                    let ly = b*r
                    
                    let o = pos + lx * u + ly * v
                    let d = Vector.normalise((px - lx) * u + (py - ly) * v - f * w)
                    mkRay o d
                
                let pixelRay x y sx sy = pointInPixel x y sx sy |> rayDirectionVector

                //shoots ray and collects list of colors sample points in pixel in the viewplane asynchronously
                let pixelColor x y = 
                    async {
                            let set = viewPlane.randomSet
                            return Array.fold(fun clrs (sx, sy) ->
                                let ray = pixelRay (float x) (float y) sx sy
                                let hit = sc.hitFunction ray
                                match hit with
                                | None -> baseClr::clrs
                                | Some(t, n, m) -> 
                                    let (o, d) = getComponents ray
                                    let p = o + t * d
                                    (m.colour sc d p n 1)::clrs 
                                ) [] set
                          }
                
                //calls pixelcolor for each pixel in image
                let ys x = [for y in 1..resY-1 -> pixelColor x y] |> Async.Parallel             
                let xs = [for x in 1..resX-1 -> ys x |> Async.RunSynchronously ] 
                
                //colors pixels image
                for x in 0..resX-2 do
                    for y in 0..resY-2 do
                        picture.SetPixel (x+1,resY-y-1, toColor (average xs.[x].[y]))
                picture