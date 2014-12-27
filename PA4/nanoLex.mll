(* CSE 130: Programming Assignment 4
 * nanoLex.mll
 * Jay Ceballos
 * A09338030
 * jjceball@ieng6.ucsd.edu 
 *)

{
  open Nano        (* nano.ml *)
  open NanoParse   (* nanoParse.ml from nanoParse.mly *)
}

rule token = parse
    eof       { EOF }

  | "("       { LPAREN }
  | ")"       { RPAREN }

  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { MUL }
  | "/"       { DIV }
  | "<"       { LT }
  | "<="      { LE }
  | "!="      { NE }
  | "&&"      { AND }
  | "||"      { OR }
  
  | "["       { LBRAC }
  | "]"       { RBRAC }
  | ";"       { SEMI }
  | "::"      { COLONCOLON }
  
  | "true"    { TRUE }   
  | "false"   { FALSE }

  | "let"     { LET }
  | "rec"     { REC }
  | "="       { EQ }
  | "in"      { IN }
  | "fun"     { FUN }
  | "->"      { ARROW }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }

  | ['0'-'9']+ as n { Num (int_of_string n) }
  | ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']* as x { Id x }
  | [' ' '\t' '\n' '\r']    { token lexbuf }
  | _           { raise (MLFailure ("Illegal Character '"^(Lexing.lexeme lexbuf)^"'")) }