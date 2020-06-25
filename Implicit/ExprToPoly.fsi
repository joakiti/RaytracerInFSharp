namespace Implicit

module ExprToPoly =

    type expr = ExprParse.expr
    val subst: expr -> (string * expr) -> expr

    type atom = ANum of float | AExponent of string * int | ARoot of atom list list * int | ADiv of atom list list * atom list list
    type atomGroup = atom list  
    type simpleExpr = SE of atomGroup list

    val exprToSimpleExpr: expr -> simpleExpr

    type poly = P of Map<int, simpleExpr>

    val simpleExprToPoly: simpleExpr -> string -> poly
    val exprToPoly: expr -> poly
