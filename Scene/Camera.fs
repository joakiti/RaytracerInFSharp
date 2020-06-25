namespace Scene

module Camera =

    open System.Drawing
    open Util
    open Sampling.Sampler
    open Colour
    open Ray
    open Point
    open Vector
    open Types
    open System.Threading.Tasks
    
    [<Interface>]
    type Camera =
        abstract member takePicture : IScene -> Bitmap
 
    type PinHoleCamera(pos : Point, zoom : float, resX : int, resY : int, s : Sampler, pixelWidth, pixelHeight, w : Vector, u : Vector, v : Vector) =
        interface Camera with
            member this.takePicture sc =

                let baseClr = mkColour 0. 0. 0.

                //calculates the center of a given pixel   
                let pointInPixel x y sx sy = ((pixelWidth * (x - (float resX/2.0)+sx)), (pixelHeight * (y - (float resY/2.0)+sy)), (-zoom)) //is the y coord correct?
    
                let rayDirectionVector (x, y, z) = //calculates the ray direction from the camera to the center of the pixel in the view plane
                    let dir = (x * u) + (y * v) + (z * w) |> normalise
                    mkRay pos dir
    
                //piping all neccessary functions to get a color for the pixel
                let pixelRay x y sx sy = pointInPixel x y sx sy |> rayDirectionVector

                let picture = new Bitmap(resX, resY)

                let pixelColor x y = 
                    async {
                            let set = s.randomSet
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

                let ys x = [for y in 1..resY-1 -> pixelColor x y] |> Async.Parallel             
                let xs = [for x in 1..resX-1 -> ys x |> Async.RunSynchronously ] 

                for x in 0..resX-2 do
                    for y in 0..resY-2 do
                        picture.SetPixel (x+1,resY-y-1, toColor (average xs.[x].[y]))
                picture


    let mkPinhole (pos : Point) (look : Point) (up : Vector) (zoom : float) (width : float) (height : float) (resX : int) (resY : int) (s : Sampler) = 
            let pixelWidth = width / float resX
            let pixelHeight = height / float resY

            let distanceVector = D look pos

            let camCoords a b = //calculates the orthonormal coordinate space
                    let w = normalise a
                    let u = normalise (b % w)
                    let v = w % u
                    (w,u,v)

            let (w, u, v) = camCoords distanceVector up //calculates the coordinate space from the above distance vector and the up vector
            PinHoleCamera(pos, zoom, resX, resY, s, pixelWidth, pixelHeight, w, u, v)