namespace Implicit

module ExprUtil =

    open ExprParse
    open ExprToPoly

    (* Variables to change Newton Raphson *)
    let error = 0.00001 // The error allowed for roots
    let convergeTimes = 10000 // The number of times Newton Raphson will run to converge
    let maxDistance = 40. // The max distance of the scene
   
    type expr = ExprParse.expr
    type poly = ExprToPoly.poly
    type simpleExpr = ExprToPoly.simpleExpr

    (* Derivation of a expression tree *)
    let rec derive e v = 
        match e with
        | FNum(_)        -> FNum 0.
        | FVar(x)        -> match x with
                            | x when x = v -> FNum 1.
                            | _ -> FNum 0.
        | FAdd(e1, e2)   -> FAdd(derive e1 v, derive e2 v)
        | FMult(e1, e2)  -> FAdd(FMult(derive e1 v, e2), FMult(e1, derive e2 v))
        | FDiv(e1, e2)   -> FDiv(FAdd(FMult(e2, derive e1 v), FMult(FNum -1., FMult(e1, derive e2 v))), FExponent(e2, 2))
        | FExponent(e,n) -> FMult(derive e v,FMult(FNum (float(n)), FExponent(e, n-1)))
        | FRoot(e, n)    -> FDiv(derive e v, FMult(FNum (float n), FExponent(FRoot(e, n), n-1))) 
    
    (* Evaluating an expression tree from map of variable (string) to value (float) *)
    let evalExpr (v:float []) e =
        let rec ee ex c =
            match ex with
            | FNum n            -> c (n)
            | FVar x            -> c (match x with 
                                     |"x" -> v.[0]
                                     |"y" -> v.[1]
                                     |"z" -> v.[2]
                                     |_ -> failwith "unknown variable in expression" )
            | FAdd(e1, e2)      -> ee e1 (fun ex1 -> ee e2 (fun ex2 -> c ((ex1) + (ex2))))
            | FMult(e1, e2)     -> ee e1 (fun ex1 -> ee e2 (fun ex2 -> c ((ex1) * (ex2))))
            | FDiv(e1, e2)      -> ee e1 (fun ex1 -> ee e2 (fun ex2 -> c ((ex1) / (ex2))))
            | FExponent(e1, n)  -> ee e1 (fun ex1 -> c ((ex1)**(float n)))
            | FRoot(e1, n)      -> ee e1 (fun ex1 -> c ((ex1)**(1.0 / (float n))))
        ee e id

    (* Evaluating simple expressions *)
    let evalAtom a (v:float []) =
        match a with
        | AExponent(x, n) -> match x with 
                             |"px" -> v.[0]**float(n)
                             |"py" -> v.[1]**float(n)
                             |"pz" -> v.[2]**float(n)
                             |"dx" -> v.[3]**float(n)
                             |"dy" -> v.[4]**float(n)
                             |"dz" -> v.[5]**float(n)
                             |_ -> failwith "unknown variable in expression"
        | ANum k -> float(k)
        | _ -> failwith "AG should not contain ADiv or ARoot at this point"      
    let evalAG ag v =
        List.fold (fun e a -> e * (evalAtom a v)) 1. ag
    let evalSE se v =
        let (SE(simpex)) = se
        List.fold (fun e ag -> e + (evalAG ag v)) 0. simpex

    (* Turning poly with x, y and z into poly with only t as unknown *)
    let simplifyPoly poly vars =
        List.foldBack(fun se a -> (evalSE se vars)::a) poly []
    
    (* Insert a value v with degree d into a polynomial *) 
    let rec insert d v poly =
        let ins i v l =
            List.foldBack (fun x (acc, c) -> if i = c then (v :: acc, c+1) else (x :: acc, c+1)) l ([], 0) |> fst
        if d = List.length poly
        then v :: poly
        elif d > List.length poly
        then insert d v (0.::poly)
        else ins d v poly
    
    (* Try to find value with degree d in polynomial *)
    let tryFind d poly = 
        let l = List.length poly - 1
        if d > l
        then None
        else Some(poly.[l-d])

    (* Deriving a simplified polynomial *)
    let derivePoly poly = 
        List.foldBack (fun n (acc, d) -> 
            if d = 0 then (acc, 1) else (n * (float d)) :: acc, d+1) poly ([], 0) |> fst
 
    (* Sturm sequence of poly *)
    let sturm p0 p1 = 
        let longDivisions poly1 poly2 = 
            let longDivision poly1 poly2 =               
                let lead p = (List.length p - 1, List.head p)// Leading term of polynomial
                let degree p = List.length p - 1// The degree of a polynomial

                // Addition of a term to a polynomial
                let add (deg, t1) poly = 
                    match tryFind deg poly with
                    | None ->  insert deg t1 poly
                    | Some t2 -> insert deg (t1+t2) poly

                // Subtraction of two polynomials
                let subtract p1 p2 =
                    let minus t1 t2 =           
                        match t1 with 
                        | None ->  -t2
                        | Some t -> t - t2
                    let x = List.foldBack (fun t2 (acc, deg) -> (minus (tryFind deg p1) t2 :: acc, deg+1)) p2 ([], 0) |> fst
                    List.foldBack (fun t1 (acc, deg) -> match tryFind deg x with 
                                                        | None -> (t1::acc, deg+1)
                                                        | Some x -> (x::acc, deg+1)
                                  ) p1 ([], 0) |> fst
                
                // Multiplication of a term with a polynomial
                let multiply (d1, t1) poly =
                    List.foldBack (fun t2 (acc, d2) -> (insert (d1+d2) (t1*t2) acc, d2+1)) poly ([], 0) |> fst
                        
                // Division of two terms
                let divide ((deg1:int), t1:float) (deg2 , t2) = (deg1 - deg2, t1/t2)
 
                // Removing values equal to (or almost equal to) 0
                let removeZero (p:float list) = List.filter (fun value -> ((abs value) > 0.0000000001) ) p
                
                // Calculating the quotient and remainder of polynomial long division
                let calcQandR poly1 poly2 =
                    let rec qr q rem dvsr = 
                        match List.isEmpty rem with
                        | true -> (q, rem)
                        | false -> 
                            if (degree rem) >= (degree dvsr)
                            then                                     
                                let (deg, t) = divide (lead rem) (lead dvsr) //Divide the first term of the dividend by the highest term of the divisor
                                let q' = add (deg, t) q //Calc new quotient
                                let product = multiply (deg, t) dvsr //Multiply the divisor by the result just obtained
                                let r = subtract rem product |> removeZero //Subtract the product just obtained from the appropriate terms of the original dividend
                                qr q' r dvsr
                            else (q, rem)
                    qr [] poly1 poly2
                calcQandR poly1 poly2

            let negatePoly m = List.map (fun value -> -value) m                   
            let onlyConstant m = List.length m = 1 
            
            // Creates the list of remainders from doing polynomial long division on two polynomials                            
            let rec remainders p1 p2 xs =
                if List.isEmpty p2 || onlyConstant p2 then xs 
                else 
                    let (_, rem) = longDivision p1 p2
                    let p3 = negatePoly rem
                    remainders p2 p3 (p3::xs)
            remainders poly1 poly2 [poly2;poly1]
        
        let sturmChain = longDivisions p0 p1
        List.filter (fun s -> (not << List.isEmpty) s) sturmChain
    
    let newtonRaphson poly map = 
        //Evaluating the polynomial, deriving it and obtaining the Sturm sequence
        let eval = simplifyPoly poly map
        let deriv = derivePoly eval
        let sturmSeq = sturm eval deriv

        // Claculating a simplified polynomial
        let calcPoly poly a = 
            List.foldBack (fun v (s, k) -> (a**float(k)*v + s, k+1)) poly (0., 0) |> fst
        
        // Find the interval with a single root
        let rec rootInterval a b = 
            let signChanges seq = 
                let initSignChange = if List.head seq > 0. then (fun x -> x < 0.) else (fun x -> x > 0.)
                List.fold (fun (n, f) x -> if f x then (n+1, (fun y -> not (f y))) else (n, f)) (0, initSignChange) (List.tail seq) |> fst
            
            // Number of roots in interval
            let numRoots a b =
                let p0 = List.map (fun t -> calcPoly t a) sturmSeq
                let p1 = List.map (fun t -> calcPoly t b) sturmSeq
                let s1 = signChanges p0
                let s2 = signChanges p1
                s1 - s2
            
            // Returns the interval with a single root if such interval exists
            let rec findRoot min max =
                let diff = max - min
                let mid = min+(diff/2.)
                match numRoots min mid  with
                | 0 ->
                    match numRoots mid max with
                    | 0                   -> None
                    | 1                   -> Some(mid, max)
                    | _ when diff < error -> Some(mid, max) //if roots are very close then return interval
                    | _                   -> findRoot mid max
                | 1                   -> Some(min, mid)
                | _ when diff < error -> Some(min, mid)
                | _                   -> findRoot min mid
            findRoot a b
        
        // Newton Raphson's method
        let nr a = a - ((calcPoly eval a) / (calcPoly deriv a))

        // Converging on a root
        let rec converge an times = 
            let an' = nr an
            if abs(an - an') < 0.00001 || times > convergeTimes
            then an'
            else converge an' (times+1)
        
        // Obtains an interval with a single root and tries to converge on it
        let rec result (min, max) = 
            match rootInterval min max with
            | None -> None
            | Some (min', max') ->
                let diff = max' - min'
                let a = min' + (diff / 2.)
                if diff < error
                then Some (a)
                else 
                    let c = converge a 0
                    if c > max' || c < min'
                    then result (min', max')
                    else Some (c)
        result (0., maxDistance)


    (* Evaluating a polynomial - different methods depending on the order *)
    let evalPoly poly vars =
        let order = List.length poly - 1
        match order with
        | 1 -> 
            let a = evalSE (poly.[0]) vars
            let b = evalSE (poly.[1]) vars
            let t = ((-1.0 * b) / a)
            if t <= 0. then None else Some t
        | 2 -> 
            let a = evalSE (poly.[0]) vars
            let b = evalSE (poly.[1]) vars
            let c = evalSE (poly.[2]) vars

            let discriminant = b * b - 4. * a * c
            match discriminant with
            | d when d < 0. -> None
            | d when d = 0. -> None
            | d -> //d > 0
              let t1 = (-b + System.Math.Sqrt(d))/(2.0 * a)
              let t2 = (-b - System.Math.Sqrt(d))/(2.0 * a)
              match (t1, t2) with
              | (d1, d2) when d1 <= 0. && d2 <= 0. -> None
              | (d1, d2) when d1 <= 0. -> Some d2
              | (d1, d2) when d2 <= 0. -> Some d1
              | (d1, d2) -> Some(min d1 d2)
        | n when n > 2 -> newtonRaphson poly vars
        | _ -> failwith "can't evaluate polynomial with nonpositive order"



    (*Print Helper Functions*)

    let isSimpleExprEmpty (SE ags) = ags = [] || ags = [[]]
    let rec ppExpr = function
      | FNum c -> string(c)
      | FVar s -> s
      | FAdd(e1,e2) -> "(" + (ppExpr e1) + " + " + (ppExpr e2) + ")"
      | FMult(e1,e2) -> (ppExpr e1) + " * " + (ppExpr e2)
      | FDiv(e1, e2) -> (ppExpr e1) + " / " + (ppExpr e2)
      | FExponent(e,n) -> "(" + (ppExpr e) + ")^" + string(n)
      | FRoot(e,n) -> "-/(" + (ppExpr e) + ")" + string(n)

    let ppAtom = function
      | ANum c -> string(c)
      | AExponent(s,1) -> s
      | AExponent(s,n) -> s+"^"+(string(n))
      | _ -> failwith "unable to print roots and division"
    let ppAtomGroup ag = String.concat "*" (List.map ppAtom ag)
    let ppSimpleExpr (SE ags) = String.concat "+" (List.map ppAtomGroup ags)

    let ppPoly v (P p) =
      let pp (d,ags) =
        let prefix = if d=0 then "" else ppAtom (AExponent(v,d))
        let postfix = if isSimpleExprEmpty ags then "" else "(" + (ppSimpleExpr ags) + ")"
        prefix + postfix
      String.concat "+" (List.map pp (Map.toList p))


