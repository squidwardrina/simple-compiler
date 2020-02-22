%{
#define IN_FILE_TYPE        ".ou"
#define OUT_FILE_TYPE       ".tok"
#define SIGNATURE           "\nRina Fridland\n"

#include <string.h>
#include "cla.tab.h"
extern int atoi(const char *);
extern void exit(int);

char lexeme[20];
char attr[20];
int line = 1;

%}

DIGIT    [0-9]
LETTER   [a-zA-Z]
ALNUM    [a-zA-Z0-9]

%x COMMENT
%%

if                          { return IF; }
else                        { return ELSE; }
while                       { return WHILE; }
int                         { return INT; }
float                       { return FLOAT; }
input                       { return INPUT; }
output                      { return OUTPUT; }
static_cast                 { return STATIC_CAST; }
switch                      { return SWITCH; }
case                        { return CASE; }
break                       { return BREAK; }
default                     { return DEFAULT; }

"=="                        { yylval.relopType = EQ; return RELOP; } 
"!="                        { yylval.relopType = NE; return RELOP; } 
"<"                         { yylval.relopType = LT; return RELOP; } 
">"                         { yylval.relopType = GT; return RELOP; } 
"<="                        { yylval.relopType = LE; return RELOP; }
">="                        { yylval.relopType = GE; return RELOP; } 

"+"                         { yylval.addopType = PLUS; return ADDOP; }
"-"                         { yylval.addopType = MINUS; return ADDOP; }

"*"                         { yylval.mulopType = MUL;  return MULOP; }
"/"                         { yylval.mulopType = DIV;  return MULOP; }

"||"                        { return OR; }
"&&"                        { return AND; }
"!"                         { return NOT; }

"{"                         |
"}"                         |
"("                         |
")"                         |
","                         |
":"                         |
";"                         |
"="                         { return yytext[0]; }

{LETTER}{ALNUM}{0,8}        { strcpy(yylval.sval, yytext); return ID; }

{DIGIT}+                    { yylval.nval.ival = atoi(yytext); return NUM; }
{DIGIT}+\.{DIGIT}+          { yylval.nval.fval = atof(yytext); return NUM; }

[\t ]+                      /* skip white space */

"/*"                        { BEGIN (COMMENT); }
<COMMENT>"*/"               { BEGIN (INITIAL); }
<COMMENT>.                  /* skip comment */

<INITIAL,COMMENT>\n         { line++; }

{LETTER}{ALNUM}{9,}         |
{DIGIT}+{LETTER}{ALNUM}*    |
.                           { strncpy(yylval.sval, yytext, MAX_LEN - 1); return UNRECOGNIZED_TOKEN; }

%%

int yywrap() { return 1; }
