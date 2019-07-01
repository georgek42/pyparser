type ident = string
[@@deriving show]

type attrs = int * int (* line_no, col_offset *)
[@@deriving show]

type boolop = And | Or
[@@deriving show]

type operator = Add | Sub | Mult | MatMult | Div | Mod | Pow | LShift
              | RShift | BitOr | BitXor | BitAnd | FloorDiv
[@@deriving show]

type unaryop = Invert | Not | UAdd | USub
[@@deriving show]

type cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
[@@deriving show]

type expr_context = Load | Store | Del | AugLoad | AugStore | Param
[@@deriving show]

type number =
  | Int of int
  | Float of float
[@@deriving show]

type slice =
  | Slice of expr option * expr option * expr option (* upper, lower, step *)
  | ExtSlice of slice list
  | Index of expr

and comprehension =
  | Comprehension of expr * expr * expr list * int
  (* target, iter, ifs, is_async *)

and except_handler =
  | ExceptionHandler of expr option * ident option * stmt list * attrs

and with_item =
  | WithItem of expr * expr option
  | Context of expr
  | OptVars of expr option

and alias =
  | Alias of ident * ident option (* name, asname *)

and keyword =
  | Keyword of ident * expr

and args =
  | Args of arg list * arg option * arg list * expr list * arg option * expr list
  (* args, vararg, kwonlyargs, kw_defaults, kwarg, defaults *)

and arg =
  | Arg of ident * expr option * attrs
  (* arg, annotation, attrs *)

and modu =
  | Module of stmt list
  | Interactive of stmt list
  | Expression of expr
  | Suite of stmt list

and stmt =
  | FunctionDef of attrs * ident * args * stmt list * expr list * expr option (* name, args, body, decorators, returns *)
  | AsyncFunctionDef of attrs * ident * args * stmt list * expr list * expr option (* name, args, body, decorators, returns *)
  | ClassDef of attrs * ident * expr list * keyword list * stmt list * expr list (* name, bases, keywords, body, decorator_list *)
  | Return of attrs * expr option
  | Delete of attrs * expr list
  | Assign of attrs * expr list * expr (* target, value *)
  | AugAssign of attrs * expr * operator * expr (* target, op, value *)
  | AnnAssign of attrs * expr * expr * expr option * int (* target, annotation, value, simple *)
  | For of attrs * expr * expr * stmt list * stmt list (* target, iter, body, else *)
  | AsyncFor of attrs * expr * expr * stmt list * stmt list (* target, iter, body, else *)
  | While of attrs * expr * stmt list * stmt list (* test, body, else *)
  | If of attrs * expr * stmt list * stmt list
  | With of attrs * with_item list * stmt list
  | AsyncWith of attrs * with_item list * stmt list
  | Raise of attrs * expr option * expr option (* exc, cause *)
  | Try of attrs * stmt list * except_handler list * stmt list * stmt list (* try, handler, else, finally *)
  | Assert of attrs * expr * expr option
  | Import of attrs * alias list
  | ImportFrom of attrs * ident option * alias list * int option (* module, names, level *)
  | Global of attrs * ident list
  | Nonlocal of attrs * ident list
  | Expr of attrs * expr
  | Pass of attrs
  | Break of attrs
  | Continue of attrs

and expr =
  | BoolOp of attrs * boolop * expr list
  | BinOp of attrs * expr * operator * expr
  | UnaryOp of attrs * unaryop * expr
  | Lambda of attrs * args * expr
  | IfExp of attrs * expr * expr * expr
  | Dict of attrs * expr list * expr list
  | Set of attrs * expr list
  | ListComp of attrs * expr * comprehension list
  | SetComp of attrs * expr * comprehension list
  | DictComp of attrs * expr * expr * comprehension list
  | GeneratorExp of attrs * expr * comprehension list
  | Await of attrs * expr
  | Yield of attrs * expr option
  | YieldFrom of attrs * expr
  | Compare of attrs * expr * cmpop list * expr list
  | Call of attrs * expr * expr list * keyword list
  | Num of attrs * number
  | Str of attrs * string
  | FormattedValue of attrs * expr * int option * expr option (* value, conversion, format_spec *)
  | JoinedStr of attrs * expr list
  | Bytes of attrs * bytes
  | NameConstant of attrs * ident
  | Ellipsis
  | Constant of attrs * ident
  | Attribute of attrs * expr * ident * expr_context
  | Subscript of attrs * expr * slice * expr_context
  | Starred of attrs * expr * expr_context
  | Name of attrs * ident * expr_context
  | List of attrs * expr list * expr_context
  | Tuple of attrs * expr list * expr_context
[@@deriving show]