%code {

#include <stdio.h>
#include <string.h>
#include <stddef.h>

extern int yylex (void);
void yyerror (const char *s);

void insert(char[], enum e_varType);
struct s_idNode* createIdNode(char*, struct s_idNode*);
void insertVarsToTable(enum e_varType, struct s_idNode*);

}

%code requires {
	#define MAX_LEN 8

    union u_numval {
         int ival;
         double fval;
    };
	enum e_relopType {EQ, NE, GT, LT, GE, LE};
	enum e_addopType {PLUS, MINUS};
	enum e_mulopType {MUL, DIV};

	enum e_varType   {INT_T, FLOAT_T};

	struct s_idNode {
		char name[MAX_LEN];
		struct s_idNode* next;
	};
	struct s_idList {
		struct s_idNode* idlistHead;
	};
}

%union {
   union u_numval nval;
   char sval[MAX_LEN];

   enum e_relopType relopType;
   enum e_addopType addopType;
   enum e_mulopType mulopType;

   enum e_varType varType; // todo: can be moved to code requires?

   struct s_idList idList;
}

%token <sval> UNRECOGNIZED_TOKEN
%left IF ELSE WHILE INT FLOAT INPUT OUTPUT
%left SWITCH CASE BREAK DEFAULT STATIC_CAST
%left OR
%left AND
%left NOT
%left <relopType> RELOP
%left <addopType> ADDOP
%left <mulopType> MULOP
%left <nval> NUM
%left <sval> ID

%type <varType> type
%type <idList> idlist

%error-verbose

%%

program: declarations stmt_block

declarations: declarations declaration
            | /* empty */

declaration: idlist ':' type ';' { insertVarsToTable($3, $1.idlistHead); }

type: INT                        { $$ = INT_T; }
    | FLOAT                      { $$ = FLOAT_T; }

idlist: idlist ',' ID            { $$.idlistHead = createIdNode($3, $1.idlistHead); }
      | ID                       { $$.idlistHead = createIdNode($1, NULL); }

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

void insert(char name[MAX_LEN], enum e_varType varType) {}

void yyerror (const char *s)
{
  extern int line;
  fprintf (stderr, "line %d: %s\n", line, s);
}

struct s_idNode* createIdNode(char *name, struct s_idNode *next) {
    struct s_idNode* head =
		(struct s_idNode*) malloc(sizeof(struct s_idNode));
    strcpy(head->name, name);
    head->next = next;
	printf("Name node added to list: %s, next: %p\n", name, next);
	return head;
}

void insertVarsToTable(enum e_varType varType, struct s_idNode *idListHead) {
	printf("\n-----> declarations started\n");

	struct s_idNode* currNode = idListHead;
	struct s_idNode* prevNode = NULL; // pointer to a used node (to free the memory)

	while (currNode != NULL) {
		insert(currNode->name, varType);
		printf("inserted: %s, typeEnum: %d\n", currNode->name, varType);

		// Move to next node & free the memory for the prev one
		prevNode = currNode;
		currNode = currNode->next;
		free(prevNode);
	}

	printf("<----- declarations ended\n");
}
