%code {

#include <stdio.h>

extern int yylex (void);
void yyerror (const char *s);
}
%code requires {
	#define MAX_LEN 8
	
    union nval_t {
         int ival;
         double fval; 
    };
	enum Relop_t {EQ, NE, GT, LT, GE, LE};
	enum Addop_t {PLUS, MINUS};
	enum Mulop_t {MUL, DIV};
}

%union {
   union nval_t nval;
   char sval[MAX_LEN];
   enum Relop_t relop_t;
   enum Addop_t addop_t;
   enum Mulop_t mulop_t;
}
 
%token <sval> UNRECOGNIZED_TOKEN
%left IF ELSE WHILE INT FLOAT INPUT OUTPUT
%left SWITCH CASE BREAK DEFAULT STATIC_CAST
%left OR 
%left AND 
%left NOT 
%left <relop_t> RELOP 
%left <addop_t> ADDOP
%left <mulop_t> MULOP
%left <nval> ID 
%left <sval> NUM

%error-verbose

%%

program: declarations stmt_block

declarations: declarations declaration
            | /* empty */

declaration: idlist ':' type ';'

type: INT
    | FLOAT

idlist: idlist ',' ID
      | ID

stmt: assignment_stmt
    | input_stmt
    | output_stmt
    | cast_stmt
    | if_stmt
    | while_stmt
    | switch_stmt
    | break_stmt
    | stmt_block

assignment_stmt: ID '=' expression ';'

input_stmt: INPUT '(' ID ')' ';'

output_stmt: OUTPUT '(' expression ')' ';'

cast_stmt: ID '=' STATIC_CAST '(' type ')' '(' expression ')' ';'

if_stmt: IF '(' boolexpr ')' stmt ELSE stmt

while_stmt: WHILE '(' boolexpr ')' stmt

switch_stmt: SWITCH '(' expression ')' '{' caselist DEFAULT ':' stmtlist '}'

caselist: caselist CASE NUM ':' stmtlist
        | /* empty */

break_stmt: BREAK ';'

stmt_block: '{' stmtlist '}'

stmtlist: stmtlist stmt
        | /* empty */

boolexpr: boolexpr OR boolterm
        | boolterm

boolterm: boolterm AND boolfactor
        | boolfactor

boolfactor: NOT '(' boolexpr ')'
          | expression RELOP expression

expression: expression ADDOP term
          | term

term: term MULOP factor
    | factor

factor: '(' expression ')'
      | ID
      | NUM

%%

int main (int argc, char **argv)
{
  extern FILE *yyin;
  if (argc != 2) {
     fprintf (stderr, "Usage: %s <input-file-name>\n", argv[0]);
	 return 1;
  }
  yyin = fopen (argv [1], "r");
  if (yyin == NULL) {
       fprintf (stderr, "failed to open %s\n", argv[1]);
	   return 2;
  }
  
  yyparse ();
  
  fclose (yyin);
  return 0;
}

void yyerror (const char *s)
{
  extern int line;
  fprintf (stderr, "line %d: %s\n", line, s);
}
