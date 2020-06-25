namespace Implicit

module ExprParse =
    [<Sealed>]

    type terminal
    type expr = 
      | FNum of float
      | FVar of string
      | FAdd of expr * expr
      | FMult of expr * expr
      | FDiv of expr * expr
      | FExponent of expr * int
      | FRoot of expr * int
    
    exception Scanerror
    exception Parseerror

    val parseStr: char seq -> expr