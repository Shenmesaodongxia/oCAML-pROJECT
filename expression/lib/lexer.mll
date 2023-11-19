{
 open Parser
 exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"


rule token = parse
 | [' ' '\t'] { token lexbuf }
 | newline { Lexing.new_line lexbuf; token lexbuf }
 | ['a'-'z' 'A'-'Z' '0'-'9' ' ' '/']+ as word { IDENT(word) }
 |"//" {line_comment lexbuf}
 | "(*" { comment lexbuf }  
 |";" {SEMI}
 |"{" {LCUR}
 |"}" {RCUR}
 |'('{LPAREN}
 |')'{RPAREN}
 |'='{EQUAL}
 |':'{Colon}
 | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
 | eof { EOF }
 
and line_comment = parse
  | '\n' | '\r' | "\r\n" { token lexbuf }  
  | _ { line_comment lexbuf }  
  | eof { EOF } 

and comment = parse
  | "*)" { token lexbuf }  
  | _    { comment lexbuf } 
  | eof  { raise (SyntaxError "Unclosed comment") }
