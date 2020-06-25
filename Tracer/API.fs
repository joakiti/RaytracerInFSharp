namespace Tracer

module API = 

  open Sampling.Sampler
  open System.Windows.Forms
  open Scene
  open Scene
  open Camera
  open ThinLensCamera
  open Transformations.Transformation
  open Shapes
  open BaseShape
  open Shape
  open BaseRectangle
  open Rectangle
  open BaseSphere
  open Sphere
  open BaseDisk
  open Disk
  open Triangle
  open Box
  open Plane
  open MatteMaterial
  open PhongMaterial
  open EmissiveMaterial
  open TransparentMaterial
  open Texture
  open Lights
  open AmbientLight
  open AmbientOccluder
  open PointLight
  open AreaLight
  open DirectionalLight
  open EnvironmentLight
  open System.Drawing
  open Util
  open Point
  open Vector
  open Colour
  open Types
  open Transformations
  open Transformation
  open KdTree.KdTree
  open Shapes.Cylinder
  open Shapes.Implicit
  open CSG
  open Parser
  open Lights.AreaLight
  
  type dummy = unit
  type vector = Vector
  type point = Point
  type colour = Colour
  type material = IMaterial
  type shape = IShape
  type baseShape = IBaseShape
  type texture = ITexture
  type camera = Camera
  type scene = IScene
  type light = ILight
  type ambientLight = IAmbientLight
  type transformation = Transformation
  type sampler = Sampler
  type Acceleration =
     /// k-d tree; default
     | KDTree
     /// regular grid
     | RegularGrid
     /// bounding volume hierarchy
     | BVH


  let mkRegularSampler (n : int) : sampler = mkRegularSampler n
  let mkRandomSampler (n : int) (sets : int) : sampler = mkRandomSampler n sets
  let mkNRooksSampler (n : int) (sets : int) : sampler =  mkNRooksSampler n sets
  let mkJitteredSampler (n : int) (sets : int) : sampler =  mkJitteredSampler n sets
  let mkMultiJitteredSampler (n : int) (sets : int) : sampler = mkMultiJitteredSampler n sets

  let mkVector (x : float) (y : float) (z : float) : vector = mkVector x y z
  let mkPoint (x : float) (y : float) (z : float) : point = mkPoint x y z
  let fromColor (c : System.Drawing.Color) : colour = fromColor c
  let mkColour (r : float) (g : float) (b : float) : colour = mkColour r g b

  let mkMatteMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) : material = MatteMaterial(ca,ka,cd,kd) :> material
  let mkPhongMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (exp : int) : material = PhongMaterial(ca,ka,cd,kd,cs,ks,exp) :> material
  let mkMatteReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) : material = MatteReflectiveMaterial(ca,ka,cd,kd,cr,kr) :> material
  let mkMatteGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cr : colour) (kr : float) (exps : int) (s : sampler) : material = mkMatteGlossyReflectiveMaterial(ca,ka,cd,kd,cr,kr,exps,s) :> material
  let mkPhongReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) : material = PhongReflectiveMaterial(ca,ka,cd,kd,cs,ks,cr,kr,exps) :> material
  let mkPhongGlossyReflectiveMaterial (ca : colour) (ka : float) (cd : colour) (kd : float) (cs : colour) (ks : float) (cr : colour) (kr : float) (exps : int) (expr : int) (s : sampler) : material = mkPhongGlossyReflectiveMaterial(ca,ka,cd,kd,cs,ks,cr,kr,exps,expr,s) :> material
  let mkEmissive (c : colour) (i : float) : material = EmissiveMaterial(c,i) :> material
  let mkTransparent (cf_in : colour) (cf_out : colour) (eta_in : float) (eta_out : float) : material = TransparentMaterial(cf_in,cf_out,eta_in,eta_out) :> material

  let mkTexture (f : float -> float -> material) : texture = Texture(f) :> texture
  let mkMatTexture (m : material) : texture = MatteTexture(m) :> texture

  let mkShape (b : baseShape) (t : texture) : shape = Shape(b,t) :>  shape
  let mkSphere (p : point) (r : float) (m : texture) : shape = mkSphere p r m
  let mkBaseSphere (p : point) (r : float)  : baseShape = BaseSphere(r) :> baseShape
  let mkBaseRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) : baseShape = BaseRectangle(topLeft,bottomRight) :> baseShape
  let mkRectangle (bottomLeft : point) (topLeft : point) (bottomRight : point) (t : texture) : shape = mkRectangle bottomLeft topLeft bottomRight t
  let mkTriangle (a:point) (b:point) (c:point) (m : material) : shape = mkTriangle a b c m :> shape
  let mkPlane (m : texture) : shape = Plane(m) :> shape
  let mkImplicit (s : string) : baseShape = mkImplicit s :> baseShape
  let mkPLY (filename : string) (smooth : bool) : baseShape = Parser.mkPLY filename smooth :> baseShape

  let mkHollowCylinder (c : point) (r : float) (h : float) (t : texture) : shape = mkHollowCylinder c r h t
  let mkSolidCylinder (c : point) (r : float) (h : float) (t : texture) (top : texture) (bottom : texture) : shape
      = mkSolidCylinder c r h t top bottom 
  let mkDisk (c : point) (r : float) (t : texture) : shape = mkDisk c r t
  let mkBaseDisk (c : point) (r : float) : baseShape = BaseDisk(r) :> baseShape
  let mkBox (low : point) (high : point) (front : texture) (back : texture) (top : texture) (bottom : texture) (left : texture) (right : texture) : shape
      = Box(low,high,front,back,top,bottom,left,right) :> shape
 

  let group (s1 : shape) (s2 : shape) : shape = group s1 s2
  let union (s1 : shape) (s2 : shape) : shape = union s1 s2
  let intersection (s1 : shape) (s2 : shape) : shape = intersection s1 s2
  let subtraction (s1 : shape) (s2 : shape) : shape = subtraction s1 s2

  let mkLight (p : point) (c : colour) (i : float) : light = PointLight(p, c, i) :> light
  let mkDirectionalLight (d : vector) (c : colour) (i : float) : light = DirectionalLight(d,c,i) :> light
  let mkAreaLight (bs : baseShape) (m : material) (s : sampler) : light = mkAreaLight bs m s :> light
  let mkEnvironmentLight (r : float) (tex : texture) (s : sampler) : light = mkEnvironmentLight r tex s :> light
  let mkAmbientLight (c : colour) (i : float) : ambientLight = AmbientLight(c, i) :> ambientLight
  let mkAmbientOccluder (c : colour) (l : float) (lmin : float) (s : sampler) : ambientLight = mkAmbientOccluder c l lmin s :> ambientLight

  let mkThinLensCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (radius : float) (fpDistance : float) (pixel : sampler) (lens : sampler) : camera = ThinLensCamera(pos, look, up, zoom, width, height, pwidth, pheight, radius, fpDistance, pixel, lens) :> camera
  let mkPinholeCamera (pos : point) (look : point) (up : vector) (zoom : float) (width : float) (height : float) (pwidth : int) (pheight : int) (s : sampler) : camera = mkPinhole pos look up zoom width height pwidth pheight s :> camera

  let mkScene (s : shape list) (l : light list) (a : ambientLight)(m : int) : scene = mkScene s l a m :> IScene
  let renderToScreen (sc : scene) (c : camera) : unit =         
        let picture = c.takePicture sc
        let frm = new Form(Visible = true, ClientSize = Size(picture.Width, picture.Height))
        let img = new PictureBox(Image = picture, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.StretchImage)
        frm.Controls.Add(img)
        frm.Show()
        Application.Run(frm)
  let renderToFile (sc : scene) (c : camera) (path : string) : unit = 
        let picture = c.takePicture sc
        picture.Save path

  let translate (x : float) (y : float) (z : float) : transformation = translate (x, y, z)
  let rotateX (angle : float) : transformation = rotateX angle
  let rotateY (angle : float) : transformation = rotateY angle
  let rotateZ (angle : float) : transformation = rotateZ angle
  let sheareXY (distance : float) : transformation = sheareXY distance
  let sheareXZ (distance : float) : transformation = sheareXZ distance
  let sheareYX (distance : float) : transformation = sheareYX distance
  let sheareYZ (distance : float) : transformation = sheareYZ distance
  let sheareZX (distance : float) : transformation = sheareZX distance
  let sheareZY (distance : float) : transformation = sheareZY distance
  let scale (x : float) (y : float) (z : float) : transformation = scale x y z
  let mirrorX : transformation = mirrorX
  let mirrorY : transformation = mirrorY
  let mirrorZ : transformation = mirrorZ
  let mergeTransformations (ts : transformation list) : transformation = mergeTransformations ts
  let transform (sh : shape) (tr : transformation) : shape = transform sh tr
  let transformLight (l : light) (t : transformation) : light = transformLight l t

  let setAcceleration (acc:Acceleration) = ()