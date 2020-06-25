namespace Implicit

module ExprUtil = 

    type expr = ExprParse.expr
    type poly = ExprToPoly.poly
    type simpleExpr = ExprToPoly.simpleExpr

    val ppSimpleExpr: simpleExpr -> string
    val ppPoly: string -> poly -> string
    val evalPoly : simpleExpr list -> float [] -> float option
    val derive : expr -> string -> expr
    val evalExpr : float [] -> expr -> float
