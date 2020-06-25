open TracerTestSuite
open Tracer.API
open System

let allTargets : Target list =
  List.concat 
    [
     Material.renderRegular;
     Material.renderMulti;
     Material.renderHigh;
     ThinLens.render;
     AreaLights.render;
     Shapes.render;
     Texture.render;
     Transparency.render;
     Shapes.render;
     AffineTransformations.render true;
     AffineTransformations.render false;
     ImplicitSurfaces.render;
     Meshes.render KDTree;
     Texture.render;
     Light.render;
     CSG.render;
     AmbientOcclusion.render [1;2;4;8;16]
     // The test groups below is only needed for teams of 7 students.
     // Teams of 6 students can uncomment the lines below.
     // Meshes.render RegularGrid;
     // Meshes.render BVH;
     ]


let renderAll (toScreen : bool) : unit = 
  List.iter (Util.renderTarget toScreen) allTargets
let renderTests (toScreen : bool) (group : string) (tests : string list) : unit = 
  Util.renderTests toScreen allTargets group tests
let renderGroups (toScreen : bool) (groups : string list) : unit =
  Util.renderGroups toScreen allTargets groups



[<EntryPoint>]
let main argv =
          
   // Util.finalize();

    Util.init();
    // To set a timeout (in seconds) for rendering each image
    // Util.setTimeout 300;

    // run all test cases
    //renderAll false;

    // To only run some test groups, use the following
    //renderGroups false ["texture"]

    // To only run some test cases of a group, use the following
    renderTests false "texture" ["boxes"] 

    Util.finalize();
    0 // return an integer exit code
