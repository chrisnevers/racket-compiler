type token =
  | TProgram
  | TInt of int
  | TChar of char
  | TBool of bool
  | TVar of string
  | TArithOp of string
  | TCmpOp of string
  | TLogOp of string
  | TRead
  | TLet
  | TIf
  | TLParen
  | TRParen
  | TLBracket
  | TRBracket
  | TPos
  | TNeg
  | TZero
  | TArray
  | TArraySet
  | TArrayRef
  | TVector
  | TVectorSet
  | TVectorRef
  | TVectorLength
  | TVoid
  | TBegin
  | TWhen
  | TUnless
  | TPrint
  | TWhile
  | TDefine
  | TColon
  | TArrow
  | TTypeInt
  | TTypeChar
  | TTypeBool
  | TTypeVoid
  | TTypeArray
  | TTypeVector
  | TTypeForAll
  | TTypeFix
  | TLambda
  | TTyLambda
  | TInst
  | TDefineType
  | TCase
  | TImport of string
  | TDot
  | TInl
  | TInr
  | TIsInl
  | TIsInr
  | TEOF

let string_of_token t =
  match t with
  | TProgram -> "Program"
  | TInt i -> string_of_int i
  | TChar ' ' -> "#\space"
  | TChar c -> "#\\" ^ (Char.escaped c)
  | TBool true -> "#t"
  | TBool false -> "#f"
  | TVar v -> v
  | TArithOp o -> o
  | TCmpOp o -> o
  | TLogOp o -> o
  | TRead -> "read"
  | TLet -> "let"
  | TIf -> "if"
  | TLParen -> "("
  | TRParen -> ")"
  | TLBracket -> "["
  | TRBracket -> "]"
  | TPos -> "pos?"
  | TNeg -> "neg?"
  | TZero -> "zero?"
  | TArray -> "array"
  | TArraySet -> "array-set!"
  | TArrayRef -> "array-ref"
  | TVector -> "vector"
  | TVectorSet -> "vector-set!"
  | TVectorRef -> "vector-ref"
  | TVectorLength -> "vector-length"
  | TVoid -> "void"
  | TBegin -> "begin"
  | TWhen -> "when"
  | TUnless -> "unless"
  | TPrint -> "print"
  | TWhile -> "while"
  | TDefine -> "define"
  | TColon -> ":"
  | TArrow -> "->"
  | TTypeInt -> "Int"
  | TTypeChar -> "Char"
  | TTypeBool -> "Bool"
  | TTypeVoid -> "Void"
  | TTypeArray -> "Array"
  | TTypeVector -> "Vector"
  | TTypeForAll -> "Forall"
  | TTypeFix -> "Fix"
  | TLambda -> "lambda"
  | TTyLambda -> "Lambda"
  | TInst -> "inst"
  | TDefineType -> "define-type"
  | TCase -> "case"
  | TImport s -> "import " ^ s
  | TDot -> "."
  | TInl -> "inl"
  | TInr -> "inr"
  | TIsInl -> "inl?"
  | TIsInr -> "inr?"
  | TEOF -> "\n"

let print_tokens tokens =
  List.iter (fun t -> print_string ((string_of_token t) ^ " ")) tokens;
  print_string "\n";