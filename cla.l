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

void printToken(char*);
void saveLexeme(char*);
void createOutFileName(char*, char*);

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

"=="                        { yylval.relop_t = EQ; return RELOP; } 
"!="                        { yylval.relop_t = NE; return RELOP; } 
"<"                         { yylval.relop_t = LT; return RELOP; } 
">"                         { yylval.relop_t = GT; return RELOP; } 
"<="                        { yylval.relop_t = LE; return RELOP; }
">="                        { yylval.relop_t = GE; return RELOP; } 

"+"                         { yylval.addop_t = PLUS; return ADDOP; }
"-"                         { yylval.addop_t = MINUS; return ADDOP; }

"*"                         { yylval.mulop_t = MUL;  return MULOP; }
"/"                         { yylval.mulop_t = DIV;  return MULOP; }

"||"                        { return OR; }
"&&"                        { return AND; }
"!"                         { return NOT; }

"{"                         { return '{'; }
"}"                         { return '}'; }
"("                         { return '('; }
")"                         { return ')'; }
","                         { return ','; }
":"                         { return ':'; }
";"                         { return ';'; }
"="                         { return '='; }

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
.                           { return '-'; }

%%

int yywrap() { return 1; }

/* int main (int argc, char **argv)
{
   char* inputFile = argv[1];
   if (argc != 2 || strlen(inputFile) < 3 || strcmp(inputFile + strlen(inputFile) - strlen(IN_FILE_TYPE), IN_FILE_TYPE)) {
      fprintf(stderr, "Usage: %s <ou file>\n", argv[0]);
      exit (1);
   }

   // Open input file
   if ((yyin = fopen(inputFile, "r")) == NULL) {
       fprintf(stderr, "Error opening input file");
       exit(1);
   }

   // Open output file
   char outFileName[255];
   createOutFileName(inputFile, outFileName);
   if ((yyout = fopen(outFileName, "w")) == NULL) {
       fprintf(stderr, "Error opening output file");
       exit(1);
   }

   fprintf(yyout, "%-5s%-15s%-10s%s\n", "LINE", "TOKEN", "LEXEME", "ATTRIBUTE");
   fprintf(yyout, "%-5s%-15s%-10s%s\n", "----", "-----", "------", "---------");

   int token;
   attr[0] = '\0';
   lexeme[0] = '\0';
   while ((token = yylex()) != 0) {
     switch (token) {
        case IF         : printToken("IF"); break;
        case ELSE       : printToken("ELSE"); break;
        case WHILE      : printToken("WHILE"); break;
        case INT        : printToken("INT"); break;
        case FLOAT      : printToken("FLOAT"); break;
        case INPUT      : printToken("INPUT"); break;
        case OUTPUT     : printToken("OUTPUT"); break;
        case STATIC_CAST: printToken("STATIC_CAST"); break;
        case SWITCH     : printToken("SWITCH "); break;
        case CASE       : printToken("CASE"); break;
        case BREAK      : printToken("BREAK"); break;
        case DEFAULT    : printToken("DEFAULT"); break;
        case RELOP      : printToken("RELOP"); break;
        case ADDOP      : printToken("ADDOP"); break;
        case MULOP      : printToken("MULOP"); break;
        case '{'        : printToken("OPEN_PAR"); break;
        case '}'        : printToken("CLOSE_PAR"); break;
        case '('        : printToken("OPEN_BRACE"); break;
        case ')'        : printToken("CLOSE_BRACE"); break;
        case ','        : printToken("COMMA"); break;
        case ':'        : printToken("COLON"); break;
        case ';'        : printToken("SEMICOLON"); break;
        case '='        : printToken("EQUAL"); break;
        case OR         : printToken("OR"); break;
        case AND        : printToken("AND"); break;
        case NOT        : printToken("NOT"); break;
        case ID         : printToken("ID"); break;
        case NUM        : printToken("NUM"); break;

        default: 
            fprintf(stderr, "Error on line %d: '%s'\n", line, lexeme);
            fprintf(yyout, "(!) Error on line %d: '%s'\n", line, lexeme);
     }
     attr[0] = '\0';
     lexeme[0] = '\0';
   }
   
   printf(SIGNATURE);
   fprintf(yyout, SIGNATURE);

   fclose(yyin);
   fclose(yyout);
   return 0;
} */

void printToken(char* token) {
    fprintf(yyout, "%-5d%-15s%-10s%s\n", line, token, lexeme, attr);
}

void saveLexeme(char* currLexeme) {
    strcpy(lexeme, currLexeme);
}

void createOutFileName(char* inFileName, char* outFileName) {
   int filenameLen = strlen(inFileName) - strlen(IN_FILE_TYPE);
   memcpy(outFileName, inFileName, filenameLen);
   outFileName[filenameLen] = '\0';
   strcat(outFileName, OUT_FILE_TYPE);
}