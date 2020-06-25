namespace Implicit

module ExprParse =

    type terminal = Add | Sub | Mul | Div | Pwr | Root | Lpar | Rpar | Int of int | Float of float | Var of string

    let isblank c = System.Char.IsWhiteSpace c
    let notblank c = not (isblank c)
    let isdigit c  = System.Char.IsDigit c
    let isletter c = System.Char.IsLetter c
    let isletterdigit c = System.Char.IsLetterOrDigit c

    let trim s = Seq.filter (notblank) s
    let explode s = [for c in (s) -> c]

    let floatval (c:char) = float((int)c - (int)'0')
    let intval(c:char) = (int)c - (int)'0'

    exception Scanerror

    let rec scnum (cs, value) = 
      match cs with 
        '.' :: c :: cr when isdigit c -> scfrac(c :: cr, (float)value, 0.1)
      | c :: cr when isdigit c -> scnum(cr, 10* value + intval c)
      | _ -> (cs,Int value)    (* Number without fraction is an integer. *)
    and scfrac (cs, value, wt) =
      match cs with
        c :: cr when isdigit c -> scfrac(cr, value+wt*floatval c, wt/10.0)
      | _ -> (cs, Float value)

    let scan s : terminal list =
      let rec sc f cs = 
        match cs with
        | [] -> f []
        | '+' :: cr -> sc (fun res -> f (Add::res)) cr
        | '*' :: cr -> sc (fun res -> f (Mul::res)) cr
        | '_' :: cr -> sc (fun res -> f (Root:: res)) cr
        | '^' :: cr -> sc (fun res -> f (Pwr::res)) cr
        | '/' :: cr -> sc (fun res -> f (Div::res)) cr
        | '(' :: cr -> sc (fun res -> f (Lpar:: res)) cr    
        | ')' :: cr -> sc (fun res -> f (Rpar::res)) cr
        | '-' :: c :: cr when isdigit c -> let (cs1, t) = scnum(cr, intval c)
                                           match t with
                                           | Float x -> sc (fun res -> f (Float (-1. * x)::res)) cs1
                                           | Int x -> sc (fun res -> f (Int (-1 * x)::res)) cs1
                                           | _ -> failwith "NaN"
        | '-' :: c :: cr when isletter c -> sc (fun res -> f (Int -1 :: Mul :: Var (c.ToString()) :: res)) cr
        | '-' :: '(' :: cr ->  sc (fun res -> f (Int -1 :: Mul :: Lpar :: res)) cr
        | '-' :: c :: cr when isblank c ->  sc (fun res -> f (Sub :: res)) cr
        | c :: cr when isdigit c -> let (cs1, t) = scnum(cr, intval c) 
                                    sc (fun res -> f (t::res)) cs1
        | c :: cr when isletter c -> sc (fun res -> f (Var (c.ToString()) ::res)) cr
        | c :: cr when isblank c -> sc f cr
        | _ -> raise Scanerror
      s |> explode |> sc id

    let insertMult tl = // Inserting multiplication where needed. 
        let rec im f = function
          Float r :: Var x :: ts -> im (fun res -> f (Float r::Mul::res)) (Var x::ts)// Inserting the last element in the terminal list again so that it is evaluated correctly.
        | Var x1 :: Var x2 :: ts -> im (fun res -> f (Var x1::Mul::res)) (Var x2::ts)
        | Int i :: Var x :: ts -> im (fun res -> f (Int i::Mul::res)) (Var x::ts)
        | Float r :: Lpar :: ts -> im (fun res -> f (Float r::Mul::Lpar::res)) ts
        | Var x :: Lpar :: ts -> im (fun res -> f (Var x::Mul::Lpar::res)) ts
        | Int i :: Lpar :: ts -> im (fun res -> f (Int i::Mul::Lpar::res)) ts
        | Int k :: Pwr :: ts -> im (fun res -> f (Lpar::Int k::Rpar::Pwr::res)) ts
        | Float k :: Pwr :: ts -> im (fun res -> f (Lpar::Float k::Rpar::Pwr::res)) ts
        | Var v :: Pwr :: ts -> im (fun res -> f (Lpar::Var v::Rpar::Pwr::res)) ts
        | Float i1 :: Div :: Float i2 :: ts -> im (fun res -> f (Float (i1/i2)::res)) ts // Simplifying fractions
        | Float i1 :: Div :: Int i2 :: ts -> im (fun res -> f (Float (i1/(float i2))::res)) ts
        | Int i1 :: Div :: Float i2 :: ts -> im (fun res -> f (Float ((float i1)/i2)::res)) ts
        | Int i1 :: Div :: Int i2 :: ts -> im (fun res -> f (Float ((float i1)/(float i2))::res)) ts
        | t :: ts -> im (fun res -> f (t::res)) ts
        | [] -> f []
        im id tl
  
    type expr = 
      | FNum of float
      | FVar of string
      | FAdd of expr * expr
      | FMult of expr * expr
      | FDiv of expr * expr
      | FExponent of expr * int
      | FRoot of expr * int

    exception Parseerror

    let rec E (ts:terminal list) = (T >> Eopt) ts //implementing functions for the parser following the grammar in the assignment.
    and Eopt (ts, inval) = 
      match ts with
      | Add :: tr -> //if there is an addition then we evaluate T and Eopt.
        let (ts1, tv) = T tr
        Eopt (ts1, FAdd(inval,tv)) 
      | Sub :: tr ->
        let (ts1, tv) = T tr
        Eopt (ts1, FAdd(inval,FMult(FNum -1., tv))) 
      | _ -> (ts, inval) //else nothing is done, since the input does not match Eopt here.
    and T ts =  (F >> Topt) ts//the same as above is done for the T and F.
    and Topt (ts, inval) = 
      match ts with
      | Mul :: tr -> 
        let (ts1, fv) = F tr
        Topt (ts1, FMult(inval, fv))
      | Div :: tr ->
        let (ts1, fv) = F tr
        Topt (ts1, FDiv(inval, fv))
      | _ -> (ts, inval)
    and F ts = (P >> Fopt) ts
    and Fopt (ts, inval) = 
      match ts with
      | Pwr :: Int(x) :: tr -> (tr, FExponent(inval, x))
      | Root :: Int(x) :: tr -> (tr, FRoot(inval,x))
      | _ -> (ts, inval)
    and P ts = //matches with an int, float, var or a left parantheses.
      match ts with
      | Int(x) :: tr -> (tr, FNum (float x))
      | Float(x) :: tr -> (tr, FNum x)
      | Var(x) :: tr -> (tr, FVar x)
      | Lpar :: tr -> //evaluates E
        let (ts1, ev) = E tr
        match ts1 with
        | Rpar :: tr -> (tr, ev) //expects the next element to be a right parantheses.
        | _ -> raise Parseerror //otherwise raise an exception as the input does not match the grammar.
      | _ -> raise Parseerror

    let parse ts = 
      match E ts with
        ([], result) -> result
      | _ -> raise Parseerror

    let parseStr s = (scan >> insertMult >> parse) s