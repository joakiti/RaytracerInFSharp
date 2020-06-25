namespace Util

module Matrix =

    open Point
    open Vector

    //
    // All matrices are assumed to be 4x4 - no check is made because of optimization
    //
    //#load "Point.fs"
    open System
    open Point

    type Matrix = {values: float array} with
            member this.getColFromIndex index = [|for i in index .. 4 .. 15 do yield this.values.[i]|]
            member this.getRowFromIndex index = [|for i = index*4 to (index*4)+3 do yield this.values.[i]|]

    let mkMatrix (values : float array) : Matrix = 
        if values.Length <> 16 then failwith "Matrices only accepts 4x4 matrices (length 16 arrays)"
        else {values=values}

    let matrix = mkMatrix [| for i in 1. .. 16. -> i|]

    let transpose (m2 : Matrix) : Matrix = mkMatrix [|m2.values.[0]; m2.values.[4]; m2.values.[8]; m2.values.[12];
                                                     m2.values.[1]; m2.values.[5]; m2.values.[9]; m2.values.[13];
                                                     m2.values.[2]; m2.values.[6]; m2.values.[10]; m2.values.[14];
                                                     m2.values.[3]; m2.values.[7]; m2.values.[11]; m2.values.[15]|]



    //R.I.P To The Beautiful Oneliners that did not make the cut
    //(transpose) (Array.concat [(m.getColFromIndex 0); m.getColFromIndex 1; m.getColFromIndex 2; m.getColFromIndex 3])
    //(matrixSum) Array.zip row col |> Array.map (fun (x,y) -> x * y) |> Array.sum  

        
    let multiply (m1 : Matrix) (m2 : Matrix) : Matrix = 
        mkMatrix [|
            (m1.values.[0]*m2.values.[0]) + (m1.values.[1]*m2.values.[4]) + (m1.values.[2]*m2.values.[8]) + (m1.values.[3]*m2.values.[12]);
            (m1.values.[0]*m2.values.[1]) + (m1.values.[1]*m2.values.[5]) + (m1.values.[2]*m2.values.[9]) + (m1.values.[3]*m2.values.[13]);
            (m1.values.[0]*m2.values.[2]) + (m1.values.[1]*m2.values.[6]) + (m1.values.[2]*m2.values.[10]) + (m1.values.[3]*m2.values.[14]);
            (m1.values.[0]*m2.values.[3]) + (m1.values.[1]*m2.values.[7]) + (m1.values.[2]*m2.values.[11]) + (m1.values.[3]*m2.values.[15]);
            (m1.values.[4]*m2.values.[0]) + (m1.values.[5]*m2.values.[4]) + (m1.values.[6]*m2.values.[8]) + (m1.values.[7]*m2.values.[12]);
            (m1.values.[4]*m2.values.[1]) + (m1.values.[5]*m2.values.[5]) + (m1.values.[6]*m2.values.[9]) + (m1.values.[7]*m2.values.[13]);
            (m1.values.[4]*m2.values.[2]) + (m1.values.[5]*m2.values.[6]) + (m1.values.[6]*m2.values.[10]) + (m1.values.[7]*m2.values.[14]);
            (m1.values.[4]*m2.values.[3]) + (m1.values.[5]*m2.values.[7]) + (m1.values.[6]*m2.values.[11]) + (m1.values.[7]*m2.values.[15]);
            (m1.values.[8]*m2.values.[0]) + (m1.values.[9]*m2.values.[4]) + (m1.values.[10]*m2.values.[8]) + (m1.values.[11]*m2.values.[12]);
            (m1.values.[8]*m2.values.[1]) + (m1.values.[9]*m2.values.[5]) + (m1.values.[10]*m2.values.[9]) + (m1.values.[11]*m2.values.[13]);
            (m1.values.[8]*m2.values.[2]) + (m1.values.[9]*m2.values.[6]) + (m1.values.[10]*m2.values.[10]) + (m1.values.[11]*m2.values.[14]);
            (m1.values.[8]*m2.values.[3]) + (m1.values.[9]*m2.values.[7]) + (m1.values.[10]*m2.values.[11]) + (m1.values.[11]*m2.values.[15]);
            (m1.values.[12]*m2.values.[0]) + (m1.values.[13]*m2.values.[4]) + (m1.values.[14]*m2.values.[8]) + (m1.values.[15]*m2.values.[12]);
            (m1.values.[12]*m2.values.[1]) + (m1.values.[13]*m2.values.[5]) + (m1.values.[14]*m2.values.[9]) + (m1.values.[15]*m2.values.[13]);
            (m1.values.[12]*m2.values.[2]) + (m1.values.[13]*m2.values.[6]) + (m1.values.[14]*m2.values.[10]) + (m1.values.[15]*m2.values.[14]);
            (m1.values.[12]*m2.values.[3]) + (m1.values.[13]*m2.values.[7]) + (m1.values.[14]*m2.values.[11]) + (m1.values.[15]*m2.values.[15])|]


    let multiplyPoint (m1 : Matrix) (m2 : Point) = 
        let (x,y,z) = getCoord m2
        mkPoint (m1.values.[0] * x + m1.values.[1] * y + m1.values.[2]* z + m1.values.[3])
                (m1.values.[4] * x + m1.values.[5] * y + m1.values.[6]* z + m1.values.[7])
                (m1.values.[8] * x + m1.values.[9] * y + m1.values.[10] * z + m1.values.[11])

    let multiplyVector (m1 : Matrix) (m2 : Vector) =
        let (x,y,z) = Vector.getCoord m2
        Vector.mkVector (m1.values.[0] * x + m1.values.[1] * y + m1.values.[2]* z)
                        (m1.values.[4] * x + m1.values.[5] * y + m1.values.[6]* z)
                        (m1.values.[8] * x + m1.values.[9] * y + m1.values.[10] * z)

    let multiplyVar (m : Matrix) ( f : float ) = mkMatrix [| for i in m.values do yield i*f |]
    type Matrix with
        static member ( * ) (mx, my) = multiply mx my //might have to change order?
        static member ( * ) (mx, p : Point) = multiplyPoint mx p
        static member ( * ) (mx, p : float) = multiplyVar mx p
        static member ( * ) (mx, v : Vector) = multiplyVector mx v

        
    
    let mkIdentityMatrix : Matrix = mkMatrix [|1.;0.;0.;0.;
                                            0.;1.;0.;0.;
                                            0.;0.;1.;0.;
                                            0.;0.;0.;1.|]
      
    let mkIdentityMatrixWithTranslateArgs (x : float, y : float, z : float) : Matrix = mkMatrix [|1.;0.;0.;x;
                                                                                                0.;1.;0.;y;
                                                                                                0.;0.;1.;z;
                                                                                                0.;0.;0.;1.|]
    let anglePI = System.Math.PI/180.

    let convertAngleToRadians ( degree : float ) : float = degree * anglePI  
    let translateInv (x : float, y : float, z : float) : Matrix = mkIdentityMatrixWithTranslateArgs(-x,-y,-z)

    translateInv (10.,10.,10.)

    let ten = 10.
        
    let nten = -ten

    let translate (x : float, y : float, z : float) = mkIdentityMatrixWithTranslateArgs(x,y,z)

    let rotateX (radians : float) : Matrix = 
        //let radians = convertAngleToRadians angle 
        mkMatrix [|1.;0.;0.;0.; //Angles are expected in radians, and should be converted using the convertDegreeToRadius method.
                   0.;(Math.Cos radians);-(Math.Sin radians);0.;
                   0.;(Math.Sin radians);(Math.Cos radians);0.;
                   0.;0.;0.;1.|]
                                                   
    
    let rotateXInv (radians : float) : Matrix = 
        //let radians = convertAngleToRadians angle 
        mkMatrix [|1.;0.;0.;0.;
                    0.;(Math.Cos radians);(Math.Sin radians);0.;
                    0.;-(Math.Sin radians);(Math.Cos radians);0.;
                    0.;0.;0.;1.|]   
                                                                
    let rotateY (radians : float) : Matrix = 
        //let radians = convertAngleToRadians angle 
        mkMatrix [|(Math.Cos radians);0.;(Math.Sin radians);0.;
                    0.;1.;0.;0.;
                    -(Math.Sin radians);0.;(Math.Cos radians);0.;
                        0.;0.;0.;1.|]

    let rotateYInv (radians : float) : Matrix = 
        //let radians = convertAngleToRadians angle 
        mkMatrix [|(Math.Cos radians);0.;-(Math.Sin radians);0.;
                0.;1.;0.;0.;
                (Math.Sin radians);0.;(Math.Cos radians);0.;
                0.;0.;0.;1.|]

    let rotateZ (radians : float) : Matrix = 
        //let radians = convertAngleToRadians angle 
        mkMatrix [|(Math.Cos radians);-(Math.Sin radians);0.;0.;
                    (Math.Sin radians);(Math.Cos radians);0.;0.;
                    0.;0.;1.;0.;
                    0.;0.;0.;1.|]

    let rotateZInv (radians : float) : Matrix = 
        //let radians = convertAngleToRadians angle 
        mkMatrix [|(Math.Cos radians);(Math.Sin radians);0.;0.;
            -(Math.Sin radians);(Math.Cos radians);0.;0.;
            0.;0.;1.;0.;
            0.;0.;0.;1.|]

    let sheare (Syx : float) (Szx : float) (Sxy : float) (Szy : float) (Sxz : float) (Syz : float) : Matrix = mkMatrix [|1.;Syx;Szx;0.;
                                                                                                                                Sxy;1.;Szy; 0.;
                                                                                                                                Sxz;Syz;1.;0.;
                                                                                                                                0.;0.;0.;1.|]
                                                                                                                            //                      ¯\_(ツ)_/¯
    let invSheare (Syx : float) (Szx : float) (Sxy : float) (Szy : float) (Sxz : float) (Syz : float) : Matrix = 
        let D = 1.-Sxy*Syx-Sxz*Szx-Syz*Szy+Sxy*Syz*Szx+Sxz*Syx*Szy
        mkMatrix [|(1.-Syz*Szy);(-Syx + Syz*Szx); (-Szx + Syx*Szy); 0.;
            (-Sxy + Sxz*Szy);(1.-Sxz*Szx); (-Szy+Sxy*Szx); 0.;
            (-Sxz + Sxy*Syz);(-Syz+Sxz*Syx); (1.-Sxy*Syx); 0.;
            0.;0.;0.; D|] * (1./D)
    let sheareYX (distance : float) : Matrix = sheare distance 0. 0. 0. 0. 0.
    let sheareZX (distance : float) : Matrix = sheare 0. distance 0. 0. 0. 0.
    let sheareXY (distance : float) : Matrix = sheare 0. 0. distance 0. 0. 0.
    let sheareZY (distance : float) : Matrix = sheare 0. 0. 0. distance 0. 0.
    let sheareXZ (distance : float) : Matrix = sheare 0. 0. 0. 0. distance 0.
    let sheareYZ (distance : float) : Matrix = sheare 0. 0. 0. 0. 0. distance

    let invSheareYX (distance : float) : Matrix = invSheare distance 0. 0. 0. 0. 0.
    let invSheareZX (distance : float) : Matrix = invSheare 0. distance 0. 0. 0. 0.
    let invSheareXY (distance : float) : Matrix = invSheare 0. 0. distance 0. 0. 0.
    let invSheareZY (distance : float) : Matrix = invSheare 0. 0. 0. distance 0. 0.
    let invSheareXZ (distance : float) : Matrix = invSheare 0. 0. 0. 0. distance 0.
    let invSheareYZ (distance : float) : Matrix = invSheare 0. 0. 0. 0. 0. distance
    
    
    let scale (x : float) (y : float) (z : float) : Matrix = mkMatrix [|x;0.;0.;0.;
                                                                        0.;y;0.;0.;
                                                                        0.;0.;z;0.;
                                                                        0.;0.;0.;1.|]
    let scaleInv (x : float) (y : float) (z : float) : Matrix = mkMatrix [|1./x;0.;0.;0.;
                                                                            0.;1./y;0.;0.;
                                                                            0.;0.;1./z;0.;
                                                                            0.;0.;0.;1.|]
    let mirrorX : Matrix = mkMatrix [|-1.;0.;0.;0.; //mirrors are their own inverse?
                                                0.;1.;0.;0.;
                                                0.;0.;1.;0.;
                                                0.;0.;0.;1.|]

    let mirrorY : Matrix = mkMatrix [|1.;0.;0.;0.;
                                        0.;-1.;0.;0.;
                                        0.;0.;1.;0.;
                                        0.;0.;0.;1.|]

    let mirrorZ : Matrix = mkMatrix [|1.;0.;0.;0.;
                                        0.;1.;0.;0.;
                                        0.;0.;-1.;0.;
                                        0.;0.;0.;1.|]

    let orthMatrix (u:Vector,v:Vector,w:Vector) : Matrix = mkMatrix [|Vector.getX u; Vector.getX v; Vector.getX w; 0.;
                                                                             Vector.getY u; Vector.getY v; Vector.getY w; 0.;
                                                                             Vector.getZ u; Vector.getZ v; Vector.getZ w; 0.;
                                                                             0.; 0.; 0.; 1.|]

    let invOrthMatrix (m:Matrix) : Matrix = transpose m