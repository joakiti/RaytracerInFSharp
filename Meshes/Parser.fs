namespace Parser

module Parser = 

    #if INTERACTIVE
    #r @"C:\Users\Mikkel\Desktop\SecondYearProject\RayTracer\packages\FParsec.1.0.3\lib\net40-client\FParsecCS.dll"
    #r @"C:\Users\Mikkel\Desktop\SecondYearProject\RayTracer\packages\FParsec.1.0.3\lib\net40-client\FParsec.dll"
    #endif 

    open System.IO
    open System
    open System.Drawing
    open FParsec
    open Util
    open Shapes
    open Triangle
    open System.Collections
    open Point
    open MatteMaterial
    open Util.Types
    open Point
    open Vector
    open KdTree
    open KdTree
    open TriangleMesh
    open BoundingBox
    open System.Text


    type PLYDataProperty =
        | SBYTE
        | BYTE
        | INT16
        | UINT16
        | INT
        | UINT32
        | FLOAT32
        | FLOAT
        | List of PLYDataProperty * PLYDataProperty

    type UserState = int * int * (PLYDataProperty * string) list * (PLYDataProperty * string) list// tuple for counting how many times we read properties.
    type Parser<'t> = Parser<'t, UserState>

    let makeDataPropertyFromString (s : string) =
        match s with
        | "float" -> FLOAT32
        | "float32" -> FLOAT32
        | "char" -> SBYTE
        | "int8" -> SBYTE
        | "uint8" -> BYTE
        | "uchar" -> BYTE
        | "short" -> INT16
        | "int16" -> INT16
        | "uint16" -> UINT16
        | "ushort" -> UINT16
        | "uint" -> UINT32
        | "int32" -> INT
        | "int" -> INT
        | _ -> failwith ("Unknown datatype: %s" + s)




    let mutable (low: Point) = mkPoint 0.0 0.0 0.0 
    let mutable (high: Point) = mkPoint 0.0 0.0 0.0 
    let mutable (smoothShade: Boolean) = false
    let mutable (normalForVertex: Map<int, Vector>) = Map.empty<int,Vector>
    let mutable isBigEndian = true

    let str s = pstring s
    
    let ws = spaces
    
    let parsePly : Parser<_> = str "ply" .>> ws
    
    let parseASCII : Parser<_> = str "format ascii 1.0" .>> ws

    let parseBinary : Parser<_>  = (str "format binary_big_endian 1.0" <|> str "format binary_little_endian 1.0") .>> ws
    
    let parseFloat : Parser<_> = pfloat .>> ws

    let parseVertex : Parser<_> = str "vertex" 
                                  .>> ws 
                                  >>. pint32 
                                  >>= (fun n -> updateUserState (fun (a,b,c,d) -> (n,b,c,d))) 
                                  .>> ws
    let parseDataTypes : Parser<_> =
         choice [
          str "char";
          str "int8";
          str "float32";
          str "float64";
          str "float";
          str "double";
          str "uint32";
          str "uint8";
          str "uint";
          str "int32";
          str "int";
          str "uint16";
          str "ushort";
          str "int16";
          str "short";
          str "uchar";
          ]        

    let parseFace : Parser<_> =  str "face" .>> ws >>. pint32 >>= (fun n -> updateUserState (fun (a,b,c,d) -> (a,n,c,d))) .>> ws 

    let parseElement : Parser<_> = str "element" .>> ws .>> (parseFace <|> parseVertex)

    let skipComment : Parser<_> = (str "comment" <|> str "obj_info") .>> skipRestOfLine (true) .>> ws

    let parseHeader : Parser<_> = str "end_header" .>> ws

    let parseFloats : Parser<_> = (getUserState >>= (fun (a,b,c,d) -> parray c.Length parseFloat))

    let parseSInt32 : Parser<_> = pint32 .>> ws

    let parseProperties : Parser<_> = str "property" 
                                      .>> ws 
                                      >>. pipe3 parseDataTypes ws (restOfLine false) (fun a b c -> (a,c))
                                      >>= (fun (data,name) -> 
                                           updateUserState (fun (v1,v2,list,v4) ->
                                           (v1,v2,(makeDataPropertyFromString data,name)::list,v4)))
                                      .>> ws
    let parsePropertiesBinary : Parser<_> = str "property list" 
                                            .>> ws 
                                            >>. pipe5 parseDataTypes ws parseDataTypes ws (restOfLine false) (fun a b c d e -> (a,c,e))
                                            >>= (fun (data1, data2, name) -> 
                                                   updateUserState (fun (v1,v2,v3,list) ->
                                                   (v1,v2,v3,(List(makeDataPropertyFromString data1,makeDataPropertyFromString data2),name)::list)))
                                            .>> ws
    let parseIndex : Parser<_> = satisfyL (fun n -> n = '3' && n = '3') "This PLY parser accepts only 3 points for triangles"  
                                 .>> ws 
                                 >>. tuple3 parseSInt32 parseSInt32 parseSInt32 
                                .>> ws //Implement failwith for 3
    
    let ASCII_Parser = 
        parsePly
        >>. parseASCII
        >>. many skipComment //TODO: Done
        >>. parseElement //TODO: Done
        >>. many parseProperties //TODO: eat shit
        >>. parseElement //TODO: Done
        >>. many (str "property" >>. skipRestOfLine (true) >>. ws) //Not sure if i need this data anyways..
        >>. parseHeader //TODO: Done
        >>. tuple2 (getUserState >>= (fun (a,b,c,d) -> parray a parseFloats)) (getUserState >>= (fun (a,b,c,d) -> parray b parseIndex))        
        //TODO: Done
    let BINARY_Parser =
        parsePly
        >>. parseBinary
        >>. many skipComment //TODO: Done
        >>. parseElement //TODO: Done
        >>. many parseProperties //TODO: eat shit
        >>. parseElement //TODO: Done
        >>. parsePropertiesBinary
        >>. parseHeader
        >>. getPosition >>= (fun p -> preturn (p.Line))

    let getIndexesOfElements (list : string list) =
        let x = List.findIndex (fun s -> s.Equals "x") list
        let y = List.findIndex (fun s -> s.Equals "y") list
        let z = List.findIndex (fun s -> s.Equals "z") list
        let nx = List.tryFindIndex (fun s -> s.Equals "nx") list
        let ny = List.tryFindIndex (fun s -> s.Equals "ny") list
        let nz = List.tryFindIndex (fun s -> s.Equals "nz") list
        let u = List.tryFindIndex (fun s -> s.Equals "u") list
        let v = List.tryFindIndex (fun s -> s.Equals "v") list
        match (u,v) with
        | (Some(u),Some(v)) -> match (nx,ny,nz) with
                              | (Some(nx),Some(ny),Some(nz)) -> [|x;y;z;nx;ny;nz;u;v|]
                              | (_,_,_) -> [|x;y;z;u;v|]
        | (_,_) -> match (nx,ny,nz) with 
                   | (Some(nx),Some(ny),Some(nz)) -> [|x;y;z;nx;ny;nz|]
                   | (_,_,_) -> [|x;y;z|]

    
    let getValues (values : float array) ( indexes : int array ) = [| for i in indexes do yield values.[i]|]

    let updatePoints (p:Point) = 
        let (px, py, pz) = Point.getCoord(p)
        let (lx, ly, lz) = Point.getCoord(low)
        let (hx, hy, hz) = Point.getCoord(high)

        let newLowX = 
                if (px < lx) then px else lx
        let newLowY =
                if (py < ly) then py else ly
        let newLowZ = 
                if (pz < lz) then pz else lz

        let newHighX = 
                if (px > hx) then px else hx
        let newHighY =
                if (py > hy) then py else hy
        let newHighZ = 
                if (pz > hz) then pz else hz
        low <- mkPoint newLowX newLowY newLowZ
        high <- mkPoint newHighX newHighY newHighZ

    let createPoint (values : float array) = let p = Point.mkPoint (values.[0]) (values.[1]) (values.[2])
                                             updatePoints p
                                             p
    let createNormal (valuesX : float array) (valuesY : float array) (valuesZ : float array)  = 
        if valuesX.Length = 6 then //We have normal values
            let v = Vector.mkVector (valuesX.[3] + 
                                  valuesY.[3] + 
                                  valuesZ.[3]) 
                                 (valuesX.[4] + 
                                  valuesY.[4] + 
                                  valuesZ.[4])
                                 (valuesX.[5] + 
                                  valuesY.[5] + 
                                  valuesZ.[5])
            Vector.normalise v
        else 
            let pa = mkPoint valuesX.[0] valuesX.[1] valuesX.[2]
            let pb = mkPoint valuesY.[0] valuesY.[1] valuesY.[2]
            let pc = mkPoint valuesZ.[0] valuesZ.[1] valuesZ.[2]
            let u = pb-pa
            let v = pc-pa
            Vector.normalise (Vector.crossProduct u v)
                                                                                        //we assume that if we have one nx, then there is also for the rest.
                                                
    let gidx = getIndexesOfElements
    let gvidx = getValues

    let getUV (triangleValues : float array) (indexOfWantedValues : int array) =
        if indexOfWantedValues.Length = 5 then
            (triangleValues.[3],triangleValues.[4])
        else (triangleValues.[6],triangleValues.[7])
            

    let generateSmoothNormals (triangleValues : float array array) 
                              (indexes : (int*int*int) array) 
                              (indexOfWantedValues : int array) = 
        for i in indexes do
            i |> (fun (x,y,z) -> let norm = createNormal (getValues triangleValues.[x] indexOfWantedValues)
                                                        (getValues triangleValues.[y] indexOfWantedValues)
                                                        (getValues triangleValues.[z] indexOfWantedValues)
                                 if Map.containsKey x normalForVertex then
                                    normalForVertex <- normalForVertex.Add (x,Vector.normalise(norm + Vector.normalise (Map.find x normalForVertex)))
                                 else 
                                    normalForVertex <- normalForVertex.Add (x,norm)
                                 if Map.containsKey y normalForVertex then
                                    normalForVertex <- normalForVertex.Add (y,Vector.normalise(norm + Vector.normalise (Map.find y normalForVertex)))
                                 else 
                                    normalForVertex <- normalForVertex.Add (y,norm)
                                 if Map.containsKey z normalForVertex then
                                    normalForVertex <- normalForVertex.Add (z,Vector.normalise(norm + Vector.normalise (Map.find z normalForVertex)))
                                 else 
                                    normalForVertex <- normalForVertex.Add (z,norm))
        
    let generateTriangles (triangleValues : float array array) (indexes : (int*int*int) array) (indexOfWantedValues : int array) = 
        if not smoothShade then
            Array.map (fun (x,y,z)-> let norm = createNormal (getValues triangleValues.[x] indexOfWantedValues)
                                                             (getValues triangleValues.[y] indexOfWantedValues)
                                                             (getValues triangleValues.[z] indexOfWantedValues)
                                     let px = createPoint (getValues triangleValues.[x] indexOfWantedValues)
                                     let py = createPoint (getValues triangleValues.[y] indexOfWantedValues)
                                     let pz = createPoint (getValues triangleValues.[z] indexOfWantedValues)
                                     if indexOfWantedValues.Length = 5 || indexOfWantedValues.Length = 8 then
                                        let (ux,vx) = getUV triangleValues.[x] indexOfWantedValues
                                        let (uy,vy) = getUV triangleValues.[y] indexOfWantedValues
                                        let (uz,vz) = getUV triangleValues.[z] indexOfWantedValues
                                        TexturedTriangle(px,py,pz, Some(norm), None, (vx,vy,vz,ux,uy,uz)) :> IBaseShape
                                     else BaseTriangle(px, py, pz, Some(norm), None) :> IBaseShape) indexes
        else 
            generateSmoothNormals triangleValues indexes indexOfWantedValues |> ignore
            Array.map (fun (x,y,z)-> let normX = (Map.find x normalForVertex)
                                     let normY = (Map.find y normalForVertex)
                                     let normZ = (Map.find z normalForVertex)
                                     let px = createPoint (getValues triangleValues.[x] indexOfWantedValues)
                                     let py = createPoint (getValues triangleValues.[y] indexOfWantedValues)
                                     let pz = createPoint (getValues triangleValues.[z] indexOfWantedValues)
                                     if indexOfWantedValues.Length = 5 || indexOfWantedValues.Length = 8 then
                                        let (ux,vx) = getUV triangleValues.[x] indexOfWantedValues
                                        let (uy,vy) = getUV triangleValues.[y] indexOfWantedValues
                                        let (uz,vz) = getUV triangleValues.[z] indexOfWantedValues
                                        TexturedTriangle(px,py,pz, None, Some(normX,normY,normZ),(vx,vy,vz,ux,uy,uz)) :> IBaseShape
                                     else 
                                     BaseTriangle(px, py, pz,None,Some(normX,normY,normZ)):> IBaseShape) indexes
   
    let findBinaryTypeAndParseFloat (dtype : PLYDataProperty) (reader: BinaryReader) : float=
        match dtype with
        | BYTE -> (float) (reader.ReadByte())
        | FLOAT32 -> let buffer = reader.ReadBytes(4)
                     if ((BitConverter.IsLittleEndian) && isBigEndian) || (not(BitConverter.IsLittleEndian) && not(isBigEndian)) then Array.Reverse(buffer)
                     (float)(BitConverter.ToSingle(buffer,0))
        | _ -> failwith "Float datatype was not recognized"

    let findBinaryTypeAndParseInt (dtype : PLYDataProperty) (reader: BinaryReader) : int=
        match dtype with
        | BYTE -> let buffer = reader.ReadBytes(1)
                  if ((BitConverter.IsLittleEndian) && isBigEndian) || (not(BitConverter.IsLittleEndian) && not(isBigEndian)) then Array.Reverse(buffer)                          
                  (int)(buffer).[0]

        | INT -> let buffer = reader.ReadBytes(4)
                 if ((BitConverter.IsLittleEndian) && isBigEndian) || (not(BitConverter.IsLittleEndian) && not(isBigEndian)) then Array.Reverse(buffer)
                 (int)(BitConverter.ToInt32(buffer, 0))

        | UINT32 -> let buffer = reader.ReadBytes(4)
                    if ((BitConverter.IsLittleEndian) && isBigEndian) || (not(BitConverter.IsLittleEndian) && not(isBigEndian)) then Array.Reverse(buffer)
                    (int)(BitConverter.ToUInt32(buffer, 0))
        | _ -> failwith "Index datatype was not recognized"
    let functionasd (property : PLYDataProperty) (reader : BinaryReader) =
        match property with 
        | List(a,b) -> (findBinaryTypeAndParseInt a reader)
                       ((findBinaryTypeAndParseInt b reader),
                       (findBinaryTypeAndParseInt b reader),
                       (findBinaryTypeAndParseInt b reader))

    let mkPLY (file : string) (smooth: Boolean) = 
        smoothShade <- smooth
        let triangles = match runParserOnFile ASCII_Parser (0,0, List.Empty, List.empty) file System.Text.ASCIIEncoding.ASCII with
                        | Success(result, us, _)   -> printfn "Success: %A; %A" result us
                                                      match us with
                                                      | (a,b,c,d) -> match result with 
                                                                   | (d,f) -> generateTriangles d f (gidx (List.map (fun (a,b) -> b) (List.rev c)))
                        | Failure(result, us, _) -> printfn "Failure: %A" result
                                                    match runParserOnFile BINARY_Parser (0,0, List.Empty, List.empty) file System.Text.ASCIIEncoding.ASCII with 
                                                    | Success(result, us, position)   -> printfn "Success: %A; %A" result us
                                                                                         match us with
                                                                                         | (a,b,c,d) -> let stream = new FileStream(file, FileMode.Open)
                                                                                                        stream.Seek (position.Index,SeekOrigin.Begin) |> ignore
                                                                                                        let reader = new BinaryReader(stream,Encoding.UTF8)
                                                                                                        let vertexes = Array.init a (fun i -> [| for dtype in (List.rev c) -> findBinaryTypeAndParseFloat (fst dtype) reader |] )
                                                                                                        let indexes = [| for i in 0 .. b -> functionasd (fst d.Head) reader|] 
                           
                                                                                                        generateTriangles vertexes indexes (gidx (List.map (fun (a,b) -> b) (List.rev c)))

                                                    | Failure(result, us, _) -> printfn "Failure: %A" result
                                                                                [||]
                                
        smoothShade <- false
        let boundingBox = mkBoundingBox low high
        low <- mkPoint 0. 0. 0.
        high <- mkPoint 0. 0. 0.
        mkTriangleMesh(triangles, Some(boundingBox))
         
    //mkPLY filepath true