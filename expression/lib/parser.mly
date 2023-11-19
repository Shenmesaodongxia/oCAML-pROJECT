%{
  open Ast
%}
%token <string> IDENT
%token LPAREN
%token RPAREN
%token EQUAL
%token Colon
%token SEMI
%token LCUR
%token RCUR
%token EOF
%start main
%type<expression list>main
%%
main:
| e=expression; EOF{ [e]}

expression:
| LPAREN; e =expression; RPAREN{e}
|nm=IDENT{ Identifier nm}

|e1=expression EQUAL e2=expression 
{ Equality(e1, e2) } 

|e1=expression; nm=IDENT
{Application(e1,Identifier nm) }

|e1=expression;LPAREN;e2=expression;RPAREN
{ Application (e1,e2)}

