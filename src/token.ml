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
  | TDefineType
  | TCase
  | TEOF

let string_of_token t =
  match t with
  | TProgram -> "Program"
  | TInt i -> "Int " ^ (string_of_int i)
  | TChar c -> "Char " ^ (Char.escaped c)
  | TBool b -> "Bool " ^ (string_of_bool b)
  | TVar v -> "Var " ^ v
  | TArithOp o -> "ArithOp " ^ o
  | TCmpOp o -> "CmpOp " ^ o
  | TLogOp o -> "LogOp " ^ o
  | TRead -> "Read"
  | TLet -> "Let"
  | TIf -> "If"
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
  | TTypeForAll -> "forall"
  | TTypeFix -> "fix"
  | TLambda -> "lambda"
  | TDefineType -> "define-type"
  | TCase -> "case"
  | TEOF -> "EOF"

let print_tokens tokens =
  List.iter (fun t -> print_string ((string_of_token t) ^ " ")) tokens;
  print_string "\n";