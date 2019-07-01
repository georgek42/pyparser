open Core
open Yojson.Basic.Util
open Ast

exception Ast_parse_error of string

let attrs_of_json (js: Yojson.Basic.json): attrs =
  let col_offset = js |> member "col_offset" |> to_int in
  let lineno = js |> member "lineno" |> to_int in
  (lineno, col_offset)

let rec arg_of_json (js: Yojson.Basic.json): arg =
  let arg = js |> member "arg" |> to_string in
  let annotation = js |> member "annotation" |> (expr_of_json |> to_option) in
  let attrs = js |> attrs_of_json in
  Arg (arg, annotation, attrs)

and args_of_json (js: Yojson.Basic.json): args =
  let args = js |> member "args" |> to_list |> List.map ~f:arg_of_json in
  let vararg = js |> member "vararg" |> (arg_of_json |> to_option) in
  let kwonlyargs = js |> member "kwonlyargs" |> to_list |> List.map ~f:arg_of_json in
  let kw_defaults = js |> member "kw_defaults" |> to_list |> List.map ~f:expr_of_json in
  let kwarg = js |> member "kwarg" |> (arg_of_json |> to_option) in
  let defaults = js |> member "defaults" |> to_list |> List.map ~f:expr_of_json in
  Args (args, vararg, kwonlyargs, kw_defaults, kwarg, defaults)

and keyword_of_json (js: Yojson.Basic.json): keyword =
  let arg = js |> member "arg" |> to_string in
  let expr = js |> member "value" |> expr_of_json in
  Keyword (arg, expr)

and operator_of_json (js: Yojson.Basic.json): operator =
  let _type = js |> member "_type" |> to_string in
  match _type with
  | "Add" -> Add
  | "Sub" -> Sub
  | "Mult" -> Mult
  | "MatMult" -> MatMult
  | "Div" -> Div
  | "Mod" -> Mod
  | "Pow" -> Pow
  | "LShift" -> LShift
  | "RShift" -> RShift
  | "BitOr" -> BitOr
  | "BitXor" -> BitXor
  | "BitAnd" -> BitAnd
  | "FloorDiv" -> FloorDiv
  | t -> raise (Ast_parse_error t)

and with_item_of_json (js: Yojson.Basic.json): with_item =
  let context_expr = js |> member "context_expr" |> expr_of_json in
  let optional_vars = js |> member "optional_vars" |> (expr_of_json |> to_option) in
  WithItem (context_expr, optional_vars)

and alias_of_json (js: Yojson.Basic.json): alias =
  let name = js |> member "name" |> to_string in
  let asname = js |> member "asname" |> to_string_option in
  Alias (name, asname)

and exception_handler_of_json (js: Yojson.Basic.json): except_handler =
  let _type = js |> member "type" |> (expr_of_json |> to_option) in
  let name = js |> member "name" |> (to_string |> to_option) in
  let body = js |> member "body" |> to_list |> List.map ~f:stmt_of_json in
  let attrs = js |> attrs_of_json in
  ExceptionHandler (_type, name, body, attrs)


and boolop_of_json (js: Yojson.Basic.json): boolop =
  let _type = js |> member "_type" |> to_string in
  match _type with
  | "And" -> And
  | "Or" -> Or
  | t -> raise (Ast_parse_error t)

and unaryop_of_json (js: Yojson.Basic.json): unaryop =
  let _type = js |> member "_type" |> to_string in
  match _type with
  | "Invert" -> Invert
  | "Not" -> Not
  | "UAdd" -> UAdd
  | "USub" -> USub
  | t -> raise (Ast_parse_error t)

and comprehension_of_json (js: Yojson.Basic.json): comprehension =
  let target = js |> member "target" |> expr_of_json in
  let iter = js |> member "iter" |> expr_of_json in
  let ifs = js |> member "ifs" |> to_list |> List.map ~f:expr_of_json in
  let is_async = js |> member "is_async" |> to_int in
  Comprehension (target, iter, ifs, is_async)

and cmpop_of_json (js: Yojson.Basic.json): cmpop =
  let _type = js |> member "_type" |> to_string in
  match _type with
  | "Eq" -> Eq
  | "NotEq" -> NotEq
  | "Lt" -> Lt
  | "LtE" -> LtE
  | "Gt" -> Gt
  | "GtE" -> GtE
  | "Is" -> Is
  | "IsNot" -> IsNot
  | "In" -> In
  | "NotIn" -> NotIn
  | t -> raise (Ast_parse_error t)

and number_of_json (js: Yojson.Basic.json): number =
  let n = js |> to_number in
  if n = snd(modf n) then Int (int_of_float n) else Float n

and slice_of_json (js: Yojson.Basic.json): slice =
  let _type = js |> member "_type" |> to_string in
  match _type with
  | "Slice" -> Slice (js |> member "lower" |> (expr_of_json |> to_option), js |> member "upper" |> (expr_of_json |> to_option), js |> member "step" |> (expr_of_json |> to_option))
  | "ExtSlice" -> ExtSlice (js |> member "dims" |> to_list |> List.map ~f:slice_of_json)
  | "Index" -> Index (js |> member "value" |> expr_of_json)
  | t -> raise (Ast_parse_error t)

and expr_context_of_json (js: Yojson.Basic.json): expr_context =
  let _type = js |> member "_type" |> to_string in
  match _type with
  | "Load" -> Load
  | "Store" -> Store
  | "Del" -> Del
  | "AugLoad" -> AugLoad
  | "AugStore" -> AugStore
  | "Param" -> Param
  | t -> raise (Ast_parse_error t)

and expr_of_json (js: Yojson.Basic.json): expr =
  let _type = js |> member "_type" |> to_string in
  let attrs = js |> attrs_of_json in
  match _type with
  | "Bool" -> BoolOp (attrs, js |> member "op" |> boolop_of_json, js |> member "values" |> to_list |> List.map ~f:expr_of_json)
  | "BinOp" -> BinOp (attrs, js |> member "left" |> expr_of_json, js |> member "op" |> operator_of_json, js |> member "right" |> expr_of_json)
  | "UnaryOp" -> UnaryOp (attrs, js |> member "op" |> unaryop_of_json, js |> member "operand" |> expr_of_json)
  | "Lambda" -> Lambda (attrs, js |> member "args" |> args_of_json, js |> member "body" |> expr_of_json)
  | "IfExp" -> IfExp (attrs, js |> member "test" |> expr_of_json, js |> member "body" |> expr_of_json, js |> member "orelse" |> expr_of_json)
  | "Dict" -> Dict (attrs, js |> member "keys" |> to_list |> List.map ~f:expr_of_json, js |> member "values" |> to_list |> List.map ~f:expr_of_json)
  | "Set" -> Set (attrs, js |> member "elts" |> to_list |> List.map ~f:expr_of_json)
  | "ListComp" -> ListComp (attrs, js |> member "elt" |> expr_of_json, js |> member "generators" |> to_list |> List.map ~f:comprehension_of_json)
  | "SetComp" -> SetComp (attrs, js |> member "elt" |> expr_of_json, js |> member "generators" |> to_list |> List.map ~f:comprehension_of_json)
  | "DictComp" -> DictComp (attrs, js |> member "key" |> expr_of_json, js |> member "value" |> expr_of_json, js |> member "generators" |> to_list |> List.map ~f:comprehension_of_json)
  | "Await" -> Await (attrs, js |> member "value" |> expr_of_json)
  | "Yield" -> Yield (attrs, js |> member "value" |> (expr_of_json |> to_option))
  | "YieldFrom" -> YieldFrom (attrs, js |> member "value" |> expr_of_json)
  | "Compare" -> Compare (attrs, js |> member "left" |> expr_of_json, js |> member "ops" |> to_list |> List.map ~f:cmpop_of_json, js |> member "comparators" |> to_list |> List.map ~f:expr_of_json)
  | "Call" -> Call (attrs, js |> member "func" |> expr_of_json, js |> member "args" |> to_list |> List.map ~f:expr_of_json, js |> member "keywords" |> to_list |> List.map ~f:keyword_of_json)
  | "Num" -> Num (attrs, js |> member "n" |> number_of_json)
  | "Str" -> Str (attrs, js |> member "s" |> to_string)
  | "FormattedValue" -> FormattedValue (attrs, js |> member "value" |> expr_of_json, js |> member "conversion" |> (to_int |> to_option), js |> member "format_spec" |> (expr_of_json |> to_option))
  | "JoinedStr" -> JoinedStr (attrs, js |> member "values" |> to_list |> List.map ~f:expr_of_json)
  | "Bytes" -> Bytes (attrs, js |> member "s" |> to_string)
  | "NameConstant" -> NameConstant (attrs, js |> member "value" |> to_string)
  | "Ellipsis" -> Ellipsis
  | "Constant" -> Constant (attrs, js |> member "value" |> to_string)
  | "Attribute" -> Attribute (attrs, js |> member "value" |> expr_of_json, js |> member "attr" |> to_string, js |> member "ctx" |> expr_context_of_json)
  | "Subscript" -> Subscript (attrs, js |> member "value" |> expr_of_json, js |> member "slice" |> slice_of_json, js |> member "ctx" |> expr_context_of_json)
  | "Starred" -> Starred (attrs, js |> member "value" |> expr_of_json, js |> member "ctx" |> expr_context_of_json)
  | "Name" -> Name (attrs, js |> member "id" |> to_string, js |> member "ctx" |> expr_context_of_json)
  | "List" -> List (attrs, js |> member "elts" |> to_list |> List.map ~f:expr_of_json, js |> member "ctx" |> expr_context_of_json)
  | "Tuple" -> Tuple (attrs, js |> member "elts" |> to_list |> List.map ~f:expr_of_json, js |> member "ctx" |> expr_context_of_json)
  | t -> raise (Ast_parse_error t)

and stmt_of_json (js: Yojson.Basic.json): stmt =
  let _type = js |> member "_type" |> to_string in
  let attrs = js |> attrs_of_json in
  match _type with
  | "Expr" -> Expr (attrs, js |> member "value" |> expr_of_json)
  | "FunctionDef" -> FunctionDef (attrs, js |> member "name" |> to_string, js |> member "args" |> args_of_json, List.map (js |> member "body" |> to_list) ~f:stmt_of_json, List.map (js |> member "decorator_list" |> to_list) ~f:expr_of_json, js |> member "returns" |> (expr_of_json |> to_option))
  | "AsyncFunctionDef" -> AsyncFunctionDef (attrs, js |> member "name" |> to_string, js |> member "args" |> args_of_json, List.map (js |> member "body" |> to_list) ~f:stmt_of_json, List.map (js |> member "decorator_list" |> to_list) ~f:expr_of_json, js |> member "returns" |> (expr_of_json |> to_option))
  | "ClassDef" -> ClassDef (attrs, js |> member "name" |> to_string, js |> member "bases" |> to_list |> List.map ~f:expr_of_json, js |> member "keywords" |> to_list |> List.map ~f:keyword_of_json, js |> member "body" |> to_list |> List.map ~f:stmt_of_json, js |> member "decorator_list" |> to_list |> List.map ~f:expr_of_json)
  | "Return" -> Return (attrs, js |> member "value" |> (expr_of_json |> to_option))
  | "Delete" -> Delete (attrs, js |> member "targets" |> to_list |> List.map ~f:expr_of_json)
  | "Assign" -> Assign (attrs, js |> member "targets" |> to_list |> List.map ~f:expr_of_json, js |> member "value" |> expr_of_json)
  | "AugAssign" -> AugAssign (attrs, js |> member "target" |> expr_of_json, js |> member "op" |> operator_of_json, js |> member "value" |> expr_of_json)
  | "AnnAssign" -> AnnAssign (attrs, js |> member "target" |> expr_of_json, js |> member "annotation" |> expr_of_json, js |> member "value" |> (expr_of_json |> to_option), js |> member "simple" |> to_int)
  | "For" -> For (attrs, js |> member "target" |> expr_of_json, js |> member "iter" |> expr_of_json, js |> member "body" |> to_list |> List.map ~f:stmt_of_json, js |> member "orelse" |> to_list |> List.map ~f:stmt_of_json)
  | "AsyncFor" -> AsyncFor (attrs, js |> member "target" |> expr_of_json, js |> member "iter" |> expr_of_json, js |> member "body" |> to_list |> List.map ~f:stmt_of_json, js |> member "orelse" |> to_list |> List.map ~f:stmt_of_json)
  | "If" -> If (attrs, js |> member "test" |> expr_of_json, js |> member "body" |> to_list |> List.map ~f:stmt_of_json, js |> member "orelse" |> to_list |> List.map ~f:stmt_of_json)
  | "With" -> With (attrs, js |> member "items" |> to_list |> List.map ~f:with_item_of_json, js |> member "body" |> to_list |> List.map ~f:stmt_of_json)
  | "AsyncWith" -> AsyncWith (attrs, js |> member "items" |> to_list |> List.map ~f:with_item_of_json, js |> member "body" |> to_list |> List.map ~f:stmt_of_json)
  | "Raise" -> Raise (attrs, js |> member "exc" |> (expr_of_json |> to_option), js |> member "clause" |> (expr_of_json |> to_option))
  | "Try" -> Try (attrs, js |> member "body" |> to_list |> List.map ~f:stmt_of_json, js |> member "handlers" |> to_list |> List.map ~f:exception_handler_of_json, js |> member "orelse" |> to_list |> List.map ~f:stmt_of_json, js |> member "finalbody" |> to_list |> List.map ~f:stmt_of_json)
  | "Assert" -> Assert (attrs, js |> member "test" |> expr_of_json, js |> member "msg" |> (expr_of_json |> to_option))
  | "Import" -> Import (attrs, js |> member "names" |> to_list |> List.map ~f:alias_of_json)
  | "ImportFrom" -> ImportFrom (attrs, js |> member "module" |> (to_string |> to_option), js |> member "names" |> to_list |> List.map ~f:alias_of_json, js |> member "level" |> (to_int |> to_option))
  | "Global" -> Global (attrs, js |> member "names" |> to_list |> List.map ~f:to_string)
  | "Nonlocal" -> Nonlocal (attrs, js |> member "names" |> to_list |> List.map ~f:to_string)
  | "Pass" -> Pass (attrs)
  | "Break" -> Break (attrs)
  | "Continue" -> Continue (attrs)
  | t -> raise (Ast_parse_error t)

and module_of_json (js: Yojson.Basic.json): modu =
  let _type = js |> member "_type" |> to_string in
  match _type with
  | "Module" -> Module (List.map (js |> member "body" |> to_list ) ~f:stmt_of_json)
  | "Interactive" -> Interactive (List.map (js |> member "body" |> to_list) ~f:stmt_of_json)
  | "Expression" -> Expression (js |> member "body" |> expr_of_json)
  | "Suite" -> Suite (List.map (js |> member "body" |> to_list) ~f:stmt_of_json)
  | t -> raise (Ast_parse_error t)

let parse src =
  Py.initialize ();
  let pyast = Py.Import.import_module "ast" in
  let pyast2json = Py.Import.import_module "ast2json" in
  let pyjson = Py.Import.import_module "json" in

  let src = Py.String.of_string src in

  let ast = Py.Module.get_function pyast "parse" [| src |] in

  let astdict = Py.Module.get_function pyast2json "ast2json" [| ast |] in

  let astjson = Py.Module.get_function pyjson "dumps" [| astdict |] in
  let astjson = Py.String.to_string astjson in

  let json = Yojson.Basic.from_string astjson in

  module_of_json json
