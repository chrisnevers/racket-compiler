type token =
  | TProgram
  | TInt of int
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
  | TTypeBool
  | TTypeVoid
  | TTypeArray
  | TTypeVector
  | TLambda
  | TEOF

let string_of_token t =
  match t with
  | TProgram -> "Program"
  | TInt i -> "Int " ^ (string_of_int i)
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
  | TTypeBool -> "Bool"
  | TTypeVoid -> "Void"
  | TTypeArray -> "Array"
  | TTypeVector -> "Vector"
  | TLambda -> "lambda"
  | TEOF -> "EOF"

let print_tokens tokens =
  List.iter (fun t -> print_string ((string_of_token t) ^ " ")) tokens;
  print_string "\n";