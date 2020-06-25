open Tracer.API
open System.Drawing
open Parser
open System
open Shapes
open System.Diagnostics
open KdTree

[<EntryPoint>]
let main argv = 
    // Sampler
    let sampler = mkRegularSampler 1
    let red = fromColor Color.Red
    let orange = fromColor Color.Orange


    // Material
<<<<<<< HEAD
    let mat = mkMatteMaterial (fromColor Color.Red) 0.9 (fromColor Color.Red) 0.9
=======
    let mat = mkMatteMaterial (fromColor Color.Orange) 0.9 (fromColor Color.Orange) 0.9
>>>>>>> cc43bfe161a3eb50ff1e38abf5cb52e41845f7a5
    let matR = mkMatteReflectiveMaterial (fromColor Color.Orange) 0.7 (fromColor Color.Orange) 0.7 (fromColor Color.Orange) 0.7 
    let phongT = mkPhongMaterial (fromColor Color.Yellow) 0.8 (fromColor Color.Yellow) 0.8 (fromColor Color.White) 0.8 10
    let phongR1 = mkPhongReflectiveMaterial (fromColor Color.White) 0.9 (fromColor Color.White) 0.8 (fromColor Color.White) 0.9 (fromColor Color.White) 0.9 10
    let phongR2 = mkPhongReflectiveMaterial (fromColor Color.Azure) 0.9 (fromColor Color.Azure) 0.9 (fromColor Color.Azure) 0.9 (fromColor Color.Azure) 0.9 20
    let phongR3 = mkPhongGlossyReflectiveMaterial (fromColor Color.Azure) 0.9 (fromColor Color.Azure) 0.9 (fromColor Color.Azure) 0.9 (fromColor Color.Azure) 0.9 20 20 sampler
    let tfMat (u:float) (v:float) = mat
    let tfRef (u:float) (v:float) = matR
    let tfPhongT (u:float) (v:float) = phongT
    let tfPhongR1 (u:float) (v:float) = phongR1
    let tfPhongR2(u:float) (v:float) = phongR2
    let tfPhongR3 (u:float) (v:float) = phongR3
    let matTex = mkTexture tfMat
    let matRef = mkTexture tfRef
    let matPhongT = mkTexture tfPhongT
    let tPhongR1 = mkTexture tfPhongR1
    let tPhongR2 = mkTexture tfPhongR2
    let tPhongR3 = mkTexture tfPhongR3
    let checkYoSelf = mkMatTexture (mkTransparent (mkColour 0.8 0.8 1.0) (mkColour 1. 1. 1.) 1.5 1.0)

    //Texture functions
    let grid x y =
      let abs' s f = if f < 0.0 then 1.0 - (f * s) else f * s
      if (int (abs' 1.0 x) + int (abs' 1.0 y)) % 2 = 0
      then mat
      else phongT

    let checkYoSelf = mkTexture grid 

    //Translations
    let trans = translate 1. 0. 0.
    let transneg = translate -1. 0. 0.

    // Spheres
    let sph1 = mkSphere (mkPoint 0. 1. 0.) 3. checkYoSelf
    let transsph = transform sph1 trans
    let sph2 = mkSphere (mkPoint 0. 0. 0.) 2. matTex
    let transsph2 = transform sph2 transneg
    let sphT = mkSphere (mkPoint 0. 0. 0.) 2. 

    let R = 1.5
    let r = 0.5
    let rs1 = "(" + (string R) + "^2" + " + " + (string r) + "^2)"
    let rs2 = "(" + (string R) + "^2" + " - " + (string r) + "^2)"
    let sx = "x^4 + 2x^2*y^2 + 2x^2*z^2 - 2*" + rs1 + "*x^2"
    let sy = "y^4 + 2y^2*z^2 + 2*" + rs2 + "*y^2"
    let sz = "z^4 - 2*" + rs1 + "*z^2"
    let sc = rs2 + "^2"
    let eqn = sx + " + " + sy + " + " + sz + " + " + sc
    System.Console.WriteLine eqn

    let factorial x = 
        if x = 0 then 1 else
        let rec fac_aux a acc =
          if a >= x then
            a * acc
          else
            fac_aux (a + 1) (a * acc)
        fac_aux 1 x

    let comb a b = 
        let x = float (factorial a) in
        let y = float (factorial b) in
        let z = float (factorial (a - b)) in
          x / (y * z)

    let rec strSum n f : string =
        if n = 0 then
          f 0
        else
          f n + " + " + (strSum (n - 1) f)

    let imp = mkImplicit eqn
    let impsh = mkShape imp matTex
    
    //Cylinders
    let hollow = mkHollowCylinder (mkPoint 0. 0. -8.) 3. 20. matPhongT
    let trans = translate 0. -2. 1.
    let rotx = rotateX (45. * Math.PI/180.)
    let rotz = rotateZ (Math.PI/3.)
    let scale = scale 20.0 20.0 20.0
    let tl = translate 0. 0.0 -3.0
    let tf = mergeTransformations [scale;tl]
    //let hollow = transform hollow scale
    let solid = mkSolidCylinder (mkPoint 0. 0. 0.) 1. 3. matPhongT matTex matTex
    let solid = transform solid scale

    //Boxes
    let box = mkBox (mkPoint -10. -10. -10.) (mkPoint 10. 10. 10.) checkYoSelf checkYoSelf checkYoSelf checkYoSelf checkYoSelf checkYoSelf 
    let box1 = mkBox (mkPoint -2. -2. -2.) (mkPoint 1. 1. 1.) matPhongT matPhongT matPhongT matPhongT matPhongT matPhongT
    let box2 = mkBox (mkPoint -1. -2. -2.) (mkPoint 2. 1. 1.) matTex matTex matTex matTex matTex matTex

    // Rotated rectangle
<<<<<<< HEAD
    let rect = mkRectangle (mkPoint -1. 0. 2.) (mkPoint -1. 2. 2.) (mkPoint 2. 0. 4.) matTex
=======
    let rect = mkRectangle (mkPoint -1. 0. 3.) (mkPoint -1. 2. 3.) (mkPoint 2. 0. 5.) matTex
>>>>>>> a4865aaeced84e2655012c46f6d23927706db7c6
    let rot = rotateX 45.
    
    // Disks
    let disk = mkDisk (mkPoint 0. 0. 0.) 1. matTex
    let disk = transform disk scale

    //Triangles 
    let triangle = mkTriangle (mkPoint 0. 0. 0.) (mkPoint -1. 1. -1.) (mkPoint 1. 1. 1.) mat
    let triangle = transform triangle scale

    //Triangle Meshes
    //let wabbit = mkPLY "../../../ply/bunny.ply" true 
    //let wabbit = mkShape wabbit matTex
    //let wabbit = transform wabbit scale

    let merged = mergeTransformations [trans;rot]
    let grouped = group transsph transsph2
    let unioned = union box1 box2
    let intersected = intersection box1 box2
    let subtracted = subtraction box1 box2
    //let rot = rotateX 45.
    //let merged = mergeTransformations [trans;rot]
    //let rect = transform rect rot
    //let i1 = transform impsh merged
    //let i2 = transform impsh transneg

    //Planes
    let plane = mkPlane matTex
    let rot = rotateX (System.Math.PI/2.)
<<<<<<< HEAD
    let tl = translate 0. -1. 0.
=======
    let tl = translate 0. -2. 0.
>>>>>>> cc43bfe161a3eb50ff1e38abf5cb52e41845f7a5
    let tf = mergeTransformations [rot;tl]
    let plane = transform plane tf

    //Palm
    let finger = mkSolidCylinder (mkPoint 0. 0. 0.) 10. 40. matTex matTex matTex
    let thumb = transform finger (translate -32. -8. 0.) 
    let index = transform finger (translate -12. 0. -8.)
    
<<<<<<< HEAD
    // Lights
    let la = mkAmbientLight (fromColor Color.White) 0.5
    let dl1  = mkDirectionalLight (mkVector 0. 0.5 1.) (fromColor Color.White) 1.
    let aol = mkAmbientOccluder (fromColor Color.White) 0.5 0.2 (mkRegularSampler 1)  
    let pl1 = mkLight (mkPoint -1. 1. 3.) (fromColor Color.White) 0.8
    let pl2 = mkLight (mkPoint 2. 8. 7.) (fromColor Color.White) 0.8
    let pl3 = mkLight (mkPoint 0. 0. 1.5) (fromColor Color.Orange) 3.
=======
    //Liv
    let basesph = mkSphere (mkPoint 0. 0. 0.) 2. matPhongT
    let a = transform basesph (translate -3. 0. -3.)
    let b = transform basesph (translate 1. 3. -1.)


    // Lights
    let la = mkAmbientLight (fromColor Color.White) 0.2 
    let dl1  = mkDirectionalLight (mkVector 0. 0.5 1.) (fromColor Color.White) 1.
    let aol = mkAmbientOccluder (fromColor Color.White) 0.5 0.2 (mkRegularSampler 1)  
    let pl1 = mkLight (mkPoint 0. 2. -2.) (fromColor Color.White) 0.8
    let pl2 = mkLight (mkPoint -5. 0. 0.) (fromColor Color.Orange) 3.
    let pl3 = mkLight (mkPoint 0. 10. 0.) (fromColor Color.Orange) 3.
>>>>>>> cc43bfe161a3eb50ff1e38abf5cb52e41845f7a5
    let pl4 = mkLight (mkPoint 0. 0. 1.5) (fromColor Color.Orange) 3.
    let pl5 = mkLight (mkPoint 0. 0. 1.5) (fromColor Color.Orange) 3.
    let pl6 = mkLight (mkPoint 0. 0. 1.5) (fromColor Color.Orange) 3.
          
    // Rendering
<<<<<<< HEAD
    let sce = mkScene [plane;rect] [pl1] la 5
    let sampler = mkRegularSampler 1
    let sampler2 = mkRegularSampler 1
    //let cam = mkPinholeCamera (mkPoint 0.0 3.5 3.5) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500 sampler
    //(pos : Point, lookat : Point, up : Vector, zoom : float, width : float, height : float, resX : int, resY : int, r : float, f : float, viewPlane : Sampler, lens : Sampler)
<<<<<<< HEAD
    let cam = mkThinLensCamera (mkPoint 0.0 0.0 5.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.00012 1.0 0.000013) 1.0 2.0 2.0 512 512 0.05 10.0 sampler sampler2;
    renderToFile sce cam @"C:\Users\Emili\Desktop\rectanglshit.png" //PATH HERE
=======
    let cam = mkThinLensCamera (mkPoint 0.0 0.0 7.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.00012 1.0 0.000013) 1.0 2.0 2.0 512 512 0.05 10.0 sampler sampler2;
    renderToFile sce cam @"C:\Users\Anders\Desktop\rectanglshit3.png" //PATH HERE
>>>>>>> a4865aaeced84e2655012c46f6d23927706db7c6
    //renderToFile scene camera @"/Users/livhborre/Git-projects/RayTracer/result/meshes/test1.png"
=======
    let sce = mkScene [plane] [pl1;pl2;pl3] la 5
    let sampler = mkRegularSampler 1
    let sampler2 = mkRegularSampler 1
    //let cam = mkPinholeCamera (mkPoint 0.0 1.0 0.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 0.0 1.0) 2.0 4.0 4.0 500 500 sampler
    //(pos : Point, lookat : Point, up : Vector, zoom : float, width : float, height : float, resX : int, resY : int, r : float, f : float, viewPlane : Sampler, lens : Sampler)
    let cam = mkThinLensCamera (mkPoint 0.0 0.0 5.0) (mkPoint 0.0 0.0 0.0) (mkVector 0.0 1.0 0.0) 1.0 2.0 2.0 512 512 0.05 10.0 sampler sampler2;
    //renderToFile sce cam @"C:\Users\Anders\Desktop\palm.png" //PATH HERE
    renderToFile sce cam @"/Users/livhborre/Git-projects/RayTracer/result/meshes/test1.png"
    
//    let white = fromColor Color.White
//    let ambientLight = mkAmbientLight (fromColor Color.White) 0.5
//    let l1 = mkLight (mkPoint 10.0 0.0 0.0) white 0.5
//    let matte = mkMatteMaterial white 1.0 white 1.0
//
//
//
//    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
//    let baseBunny = mkPLY "../../../ply/bunny.ply" true    
//    let white = fromColor Color.White 
//    let bunny = mkShape baseBunny (mkMatTexture (mkMatteMaterial white 1.0 white 1.0)) 
//    let camera = mkPinholeCamera (mkPoint 0.1 0.4 0.2) (mkPoint 0.0 0.1 0.0) (mkVector 0.0 1.0 0.0) 4.0 2.5 2.5 1000 1000 (mkRegularSampler 1);
//    let scene = mkScene [bunny] [l1] ambientLight 2
//    printfn "Construction took --> %s" (stopWatch.Elapsed.ToString())
//    let res = renderToFile scene camera @"/Users/livhborre/Git-projects/RayTracer/result/meshes/test1.png"
//    printfn "Tree took --> %s" (stopWatch.Elapsed.ToString())      
//    KdTree.cutoff <- 10
//    printfn "%i" KdTree.cutoff
          
>>>>>>> cc43bfe161a3eb50ff1e38abf5cb52e41845f7a5
    0 // return an integer exit code
    

