namespace Implicit

module ExprToPoly =

    open ExprParse

    type expr = ExprParse.expr
    type atom = ANum of float | AExponent of string * int | ARoot of atom list list * int | ADiv of atom list list * atom list list
    type atomGroup = atom list  
    type simpleExpr = SE of atomGroup list

    (* Substitute a variable with an expression *)
    let rec subst e (x,ex) =
      match e with    
      | FNum c -> FNum c
      | FVar s -> if s = x then ex else FVar s //if the variable is equal to x then we insert ex instead
      | FAdd(e1, e2) -> FAdd((subst e1 (x,ex)), subst e2 (x,ex))
      | FMult(e1, e2) -> FMult((subst e1 (x,ex)), subst e2 (x,ex))
      | FDiv(e1, e2) -> FDiv((subst e1 (x,ex)), subst e2 (x,ex))
      | FExponent(e,n) -> FExponent((subst e (x,ex)), n)
      | FRoot(e,n) -> FRoot((subst e (x,ex)), n)

    (* Combines each element of one list with each element of second list *)
    let rec combine xss = function
      | [] -> []
      | ys::yss -> List.map ((@) ys) xss @ combine xss yss
    
    (* Combines a list of element with itself n times *)
    let power n se =
        let rec pow a se = function
            | 1 -> a
            | n -> pow (combine a se) se (n-1)
        pow se se n

    (* Multiplies each atom group by -1, by inserting ANum -1. in each atom group *)
    let rec subtract se =
        List.map (fun ag -> ANum -1.::ag) se
    
    
    (* Adds an expression tree with divisions together to a single division
       This is done by traversing the expression tree in post-order
       and recursively remove simplify divisions if any are present. *)
    let rec divRemove = function
        | FNum c -> FNum c
        | FVar s -> FVar s
        | FAdd(e1, e2) -> 
            let ex1 = divRemove e1
            let ex2 = divRemove e2
            match (ex1, ex2) with
            | (FDiv(e1, e2), FDiv(e3, e4)) -> FDiv(FAdd(FMult(e1, e4), FMult(e2, e3)), FMult(e2, e4))
            | (FDiv(e1, e2), e3) -> (FDiv(FAdd(FMult(e1, e3), e2), e3))
            | (e3, FDiv(e1, e2)) -> (FDiv(FAdd(FMult(e1, e3), e2), e3))
            | _ -> FAdd(ex1, ex2)
        | FMult(e1, e2) ->
            let ex1 = divRemove e1
            let ex2 = divRemove e2
            match (ex1, ex2) with
            | (FDiv(e1, e2), FDiv(e3, e4)) -> FDiv((FMult(e1, e3)), FMult(e2, e4))
            | (FDiv(e1, e2), e3) -> (FDiv(FMult(e1, e3), e2))
            | (e3, FDiv(e1, e2)) -> (FDiv(FMult(e1, e3), e2))
            | _ -> FMult(ex1, ex2)
        | FExponent(e, n) ->
            let ex = divRemove e
            if n = 1 then ex
            else
                match ex with
                | FDiv(_, _) -> divExponent ex ex n
                | e -> FExponent(e, n)
        | FDiv(e1, e2) ->
            let ex1 = divRemove e1
            let ex2 = divRemove e2
            match (ex1, ex2) with
            | (FDiv(e1, e2), FDiv(e3, e4)) -> FDiv(FMult(e1, e4), FMult(e2, e3))
            | (FDiv(e1, e2), e3) -> FDiv(e1, FMult(e2, e3))
            | (e1, FDiv(e2, e3)) -> FDiv(FMult(e1, e3), e2)
            | _ -> FDiv(ex1, ex2)
        | FRoot(_,_) as e -> e
    and divExponent exp express n = // Calculates the result of a division in an exponent
        if n > 1 
        then
            match (exp, express) with
            | (FDiv(e1, e2), FDiv(e3, e4)) -> divExponent exp (FDiv((FMult(e1, e3)), FMult(e2, e4))) (n-1)
            | _ -> failwith "exp needs to be FDiv"
        else express
  
    (* Simplifies the expression tree to an atom group list *)
    let rec simplify = function
      | FNum c -> [[ANum c]]
      | FVar s -> [[AExponent(s,1)]]
      | FAdd(e1,e2) -> simplify e1 @ simplify e2 // Appends the atom list lists to indicate addition
      | FMult(e1,e2) -> combine (simplify e1) (simplify e2) // Using the combine function to multiply each expr in the first enclosure with each expression in the second expression following the rule of multiplication
      | FExponent(_,0) -> [[ANum 1.0]] // An expression to the power of 0 is 1
      | FExponent(e1,1) -> simplify e1 // An expression to the power of 1 is the expression
      | FExponent(e1,n) -> simplify (FMult(e1, ((FExponent(e1, (n-1)))))) // An expression to the power of n>1 is the expression times the expression to the power of n-1
      | FDiv(e1, e2) -> [[ADiv(simplify e1, simplify e2)]]
      | FRoot(e, n) -> [[ARoot(simplify e, n)]]
  
    (* Folds over an atom group to creates a map with variable 
       as key and the exponent as value
       and multiplies the constants of an atom group together to one *)
    let expAndCon ag =
        List.fold (fun (map, e) a -> 
        match a with
        |AExponent(x,n) ->
            let value = Map.tryFind x map
            match value with
            |Some i -> (map.Add (x, i + n), e)
            |None -> (map.Add (x, n), e)
        |ANum n -> (map, n*e)
        |_ -> (map, e)
        ) (Map.empty, 1.) ag

    (* Creates an atomGroup with the constant and the simplified AExponents *)
    let simplifyAtomGroup ag =
        let (map, n) = expAndCon ag
        // Folds over the map to create an AExponent for each entry in the map
        let atmList = (Map.foldBack (fun var exponent state ->
            let atm = AExponent(var, exponent)
            atm::state) map [])       
        // Inserting the roots of the atom group
        let atomList = List.fold (fun l a ->
                                  match a with
                                  |ARoot(_, _) as ar -> ar::l
                                  |_ -> l) atmList ag                                
        match n with // Return the atom list with the constant
        |0. -> List.empty
        |_ -> ANum n:: atomList

    (* Adds all atomGroups consisting only of an ANum to a single ANum *)
    let addConsts ags =
        List.fold(fun e ag ->
            match List.length ag with
            | 1 -> 
                match ag.Head with
                | ANum v -> e+v
                | _ -> e
            | _ -> e) 0. ags

    (* Creates a map with an atom group as key and the number of occurences of the atom group as value *)
    let groupAG ags =
        List.fold(fun (state:Map<atom list, float>) ag ->
            match ag with
            | x::xs when not xs.IsEmpty -> 
                match x with
                | ANum v -> 
                    let value = state.TryFind xs
                    match value with
                    |Some i -> state.Add(xs,v+i)
                    |None -> state.Add(xs,v)
                | _ -> failwith "atom group with no constant"
            | _ -> state) Map.empty ags

    (* Returns a simpleExpr with only one constant in each atomGroup and inserting only unique atomGroups multiplied by the number of occurences *)
    let simplifySimpleExpr (SE ags) =
        let ags' = List.map simplifyAtomGroup ags
        let map = groupAG ags'
        let se = Map.foldBack (fun k v list -> (ANum(v)::k)::list) map []
        let consts = addConsts ags'
        match consts with
        | 0.0 -> SE(se)
        | n -> SE([ANum(n)]::se)
    
    (* Simplifies divisions in a simple expression *)
    let rec removeDivisions se =
        match se with
        | [] -> failwith "empty simple expression" 
        | head::ag -> 
            let st = removeDiv head // Simplifies the first atom group to a single division            
            List.fold (fun (ADiv(e1, e2)) e -> // Folds over the simple expression, adding them all together to a single division
                let (ADiv(e3, e4)) = removeDiv e
                ADiv((combine e1 e4) @ (combine e2 e3), combine e2 e4)
            ) st ag
    and removeDiv = function // Multiplies all atoms of an atom group into a single division
        | [] -> failwith "empty atom group"
        | head::ag ->
        let state = // Defines a division to multiply the rest with
            match head with
            | ADiv(e1, e2) -> // Removes divisions from each subexpression
                let (ADiv(ex1, ex2)) = removeDivisions e1
                let (ADiv(ex3, ex4)) = removeDivisions e2
                let num = combine ex1 ex4 // Divides the subexpressions to a single division
                let den = combine ex2 ex3
                ADiv(num, den)
            | a -> ADiv([[a]], [[ANum 1.]])
            
        let multiplyWithDiv (ADiv(e1, e2)) = function
            | ADiv(e3, e4) -> 
                let (ADiv(ex1, ex2)) = removeDivisions e3
                let (ADiv(ex3, ex4)) = removeDivisions e4                               
                let num = combine ex1 ex4
                let den = combine ex2 ex3
                ADiv(combine e1 num, combine den e2)
            | e3 -> ADiv((combine [[e3]] e1), e2)

        List.fold (fun s a -> multiplyWithDiv s a) state ag

    (* Folds over the atom groups calling simplifyRoot *)
    let simplifyRoots (SE se) =
        let simplifyRoot ag = // Simplifies roots appearing n (root) times in the atom group
            let (map, at) = 
                List.fold (fun (map, atms) a -> // Creates a map with the root as key and number of occurences as value
                    match a with
                    |ARoot(ex, n) as x ->
                        let value = Map.tryFind x map
                        match value with
                        |Some i -> 
                            if i+1 = n 
                            then (map.Add (x, 0), removeDivisions ex::atms) // n-root appearing n times
                            else (map.Add (x, i + 1), atms)
                        |None -> (map.Add (x, 1), atms)
                    |atm -> (map, atm::atms)
                    ) (Map.empty, []) ag
            Map.fold(fun ag a n -> (List.init n (fun _ -> a)) @ ag) at map // Adds the nonsimplified roots to the atom group again
        List.fold (fun ags ag -> (simplifyRoot ag)::ags) [] se

    (* Returns the root (if any) and the nonroots of an atom group *)
    let rec getRoot ag nonroots =
        match ag with
        | [] -> (None, nonroots) // No root in atom group
        | ARoot(_, _) as a::atms -> (Some a, nonroots@atms) // Found root, return everything else
        | a::atms -> getRoot atms (a::nonroots) // Look for root in rest of atom group
    
    let rec removeRoots se =
        let rec removeRoot sexp nonroots = 
            match sexp with
            | [] -> 
                let (ADiv(num, _)) = removeDivisions nonroots
                simplifySimpleExpr (SE num) // All roots removed
            | ag::ags -> 
                match getRoot ag [] with // Look for root
                | (Some (ARoot(ex, n)), atms) -> // Found root
                
                    let left = removeDivisions ex // Removing divisions from the left side
                    let r = (nonroots@ags) // The right side of the equation               
                    let num = r |> power n // The numerator of the right side to the power of n
                    let den = if (List.isEmpty atms) then [[ANum 1.]] else [atms] |> power n // The denominator of the right side to the power of n
                
                    let totalNum = //The numerator of the left side after subtracting the right side
                        match (left, num, den) with
                        | (ADiv(e1, e2), e3, e4) -> ((combine e1 e4) @ (combine (subtract e3) e2))
                        | _ -> failwith "left has to be ADiv"

                    removeRoots (SE totalNum)        
            
                | _ -> removeRoot (ags) (ag::nonroots) // Look for root in next atom group
        
        let (ADiv(num, _)) = se |> simplifyRoots |> removeDivisions
        let (SE sexp) = simplifySimpleExpr (SE num)
        removeRoot sexp []

    let exprToSimpleExpr e = 
        match divRemove e with
        | FDiv(num, _) -> num |> simplify |> SE |> removeRoots
        | _ -> e |> simplify |> SE |> removeRoots

    type poly = P of Map<int,simpleExpr>

    (* Collect atom groups into groups with respect to one variable v *)
    let splitAG v m = function
      | [] -> m
      | ag ->
        let eqV = function AExponent(v',_) -> v = v' | _ -> false
        let addMap d ag m = // Using the exponent of v as the key in the map, we then cons atomGroups to the value of these keys (eg. t^2*x means that 2 -> x in the map)
            let ag' = Map.tryFind d m
            match ag' with
            |Some (SE se) -> 
                let atG = ag::se
                Map.add d (SE(atG)) m            
            |None -> Map.add d (SE([ag])) m
        match List.tryFind eqV ag with
          | Some (AExponent(_,d)) ->
            let ag' = List.filter (not << eqV) ag
            addMap d ag' m
          | Some _ -> failwith "splitAG: Must never come here! - ANum will not match eqV"
          | None -> addMap 0 ag m

    let simpleExprToPoly (SE ags) v =
      P (List.fold (splitAG v) Map.empty ags)

    let exprToPoly exp = 
        let ex = FAdd(FVar "px", FMult(FVar "t",FVar "dx"))
        let ey = FAdd(FVar "py", FMult(FVar "t",FVar "dy"))
        let ez = FAdd(FVar "pz", FMult(FVar "t",FVar "dz"))
        let expSubst = List.fold subst exp [("x",ex);("y",ey);("z",ez)]
        (exprToSimpleExpr >> simpleExprToPoly) expSubst "t"
