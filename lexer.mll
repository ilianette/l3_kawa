{
  open Lexer
  open Parser
  open AST

  let keywords = List.to_seq
  [ "print", PRINT
  ; "main", MAIN
  ; "class", CLASS
  ; "new", NEW
  ; "true", TRUE
  ; "false", FALSE
  ; "if", IF
  ; "else", ELSE
  ; "attribute", ATTR
  ; "method", METH
  

  
  ]
  
  let keyw_or_ident s =
    let h = Hashtbl.of_seq keywords in
    try Hashtbl.find h s
    with _ -> IDENT s
}

let digit = ['0'-'9']
let number = ['-']? digit+
let letter = ['a'-'z' 'A' - 'Z']
let ident = ['a'-'z' '_'] (letter | '_' | digit)*

rule token = parse
     | number as n { INT(int_of_string n) }
     | ident as id { keyw_or_ident id }

     | ";" { SEMI }
     | "." { DOT }
     | "(" { LPAR }
     | ")" { RPAR }
     | "{" { BEGIN }
     | "}" { END }

     | "+" { PLUS }
     | "*" { TIMES }
     | '/" { DIV }
     | "-" { NEG }

     | "=" { ASSIGN }

     | "!" { NOT }
     | "&&" { LAND }
     | "||" { LOR }
     | "==" { EQ }
     | "!=" { NEQ }
     | "<" { SGT }
     | "<=" { GT }
     | ">" { SLT }
     | ">=" { LT }

     
     
