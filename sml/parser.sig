
signature PARSER =
sig
    type id = string
    type con = string

    datatype expr
      = Var of id
      | Abs of string * expr
      | App of expr * expr
      | Let of id * expr * expr
      | Fix of id * expr
      | Literal of expr literal

    and 'a literal
      = IntLit of string
      | RealLit of string
      | TupleLit of 'a list

    datatype typeSig
      = TyCon of string
      | TyApp of typeSig * typeSig
      | TyVar of id

    type dataBody = (con * typeSig option) list

    datatype decl
      = ValDecl of (id * expr)
      | DataDecl of (con * id list * dataBody)

    type file = decl list

    exception ParseException of string

    val parseTokens : Lexer.token list -> file (* Raises ParseException *)
    val prettyExpr : expr -> string
end
