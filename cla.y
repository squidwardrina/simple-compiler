%code {

#include <stdio.h>
#include <string.h>
#include <stddef.h>

extern int yylex (void);
void yyerror (const char *s);

void insert(char[], enum e_varType);
struct s_idNode* createIdNode(char*, struct s_idNode*);
void insertVarsToTable(enum e_varType, struct s_idNode*);
void freeSymbolTable();
void copyExpInfo(struct s_expInfo, struct s_expInfo);
void performAddop(struct s_expInfo*, struct s_expInfo, enum e_addopType,struct s_expInfo);
void performMulop(struct s_expInfo*, struct s_expInfo, enum e_mulopType,struct s_expInfo);
void plus(struct s_expInfo*, struct s_expInfo, struct s_expInfo);
void minus(struct s_expInfo*, struct s_expInfo, struct s_expInfo);
void multiply(struct s_expInfo*, struct s_expInfo, struct s_expInfo);
void divide(struct s_expInfo*, struct s_expInfo, struct s_expInfo);
void assignValue(char[], struct s_expInfo);
struct s_symbol* symbolLookup(char[]);
void variableToExpression(struct s_expInfo*, char[]);

struct s_symbol {
	char name[MAX_LEN];
	enum e_varType type;
	union u_numval val;
};
struct s_symbolTableNode {
	struct s_symbol symbol;
	struct s_symbolTableNode* next;
};
struct s_symbolTableNode* g_symbolTableHead = NULL; // using globals is wrong, but first things first

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
	
	struct s_expInfo {
		enum e_varType type;
		union u_numval val;
	};

}

%union {
   union u_numval nval;
   char sval[MAX_LEN];

   enum e_relopType relopType;
   enum e_addopType addopType;
   enum e_mulopType mulopType;

   enum e_varType varType;

   struct s_idList idList;
   struct s_expInfo expInfo;
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
%left <expInfo> NUM
%left <sval> ID

%type <varType> type
%type <idList> idlist
%type <expInfo> expression term factor

%error-verbose

%%

program: declarations { printf("\n-- declarations ended --\n\n"); } stmt_block { freeSymbolTable(); }

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

assignment_stmt: ID '=' expression ';' { assignValue($1, $3); }

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

expression: expression ADDOP term { performAddop(&($$), $1, $2, $3); }
          | term                  { copyExpInfo($$, $1); }

term: term MULOP factor           { performMulop(&($$), $1, $2, $3); }
    | factor                      { copyExpInfo($$, $1); }

factor: '(' expression ')'        { copyExpInfo($$, $2); }
      | ID                        { variableToExpression(&($$), $1); }
      | NUM                       { copyExpInfo($$, $1); }

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

void insert(char name[MAX_LEN], enum e_varType varType) {
	struct s_symbolTableNode* newNode = 
		(struct s_symbolTableNode*) malloc(sizeof(struct s_symbolTableNode));
	
	// TODO: check if var already defined before inserting
	
	strcpy(newNode->symbol.name, name);
	newNode->symbol.type = varType;
	
	newNode->next = g_symbolTableHead;
	g_symbolTableHead = newNode;
	
	printf("symbol inserted to table: %s, type #%d\n", newNode->symbol.name, varType);
}

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
	return head;
}

void insertVarsToTable(enum e_varType varType, struct s_idNode *idListHead) {
	struct s_idNode* currNode = idListHead;
	struct s_idNode* prevNode = NULL; // pointer to a used node (to free the memory)

	while (currNode != NULL) {
		insert(currNode->name, varType);

		// Move to next node & free the memory for the prev one
		prevNode = currNode;
		currNode = currNode->next;
		free(prevNode);
	}
}

void freeSymbolTable() {
	struct s_symbolTableNode* currNode = g_symbolTableHead;
	struct s_symbolTableNode* prevNode = NULL;

	printf("\n---------------\nsymbol table:");
	while (currNode != NULL) {
		// Print the symbol table before deleting
		if (currNode->symbol.type == INT_T) printf("  %s=%d", currNode->symbol.name, currNode->symbol.val.ival);
		else printf("  %s=%.1f", currNode->symbol.name, currNode->symbol.val.fval);
		
		prevNode = currNode;
		currNode = currNode->next;
		free(prevNode);
	}
	printf("\nsymbol table freed\n");
}

void copyExpInfo(struct s_expInfo src, struct s_expInfo dest) {
	dest.type = src.type;
	switch (dest.type) {
		case INT_T: dest.val.ival = src.val.ival; break;
		case FLOAT_T: dest.val.fval = src.val.fval; break;
	}
}

void performMulop(struct s_expInfo* res, 
                  struct s_expInfo a, 
                  enum e_mulopType opType,
                  struct s_expInfo b) {
	switch (opType) {
		case MUL: multiply(res, a, b); break;
		case DIV: divide(res, a, b); break;
	}
	if (res->type == INT_T) printf("Performed MULOP. res = %d\n", res->val.ival);
	else printf("Performed MULOP. res = %.1f\n", res->val.fval);
}

void multiply(struct s_expInfo* res, struct s_expInfo a, struct s_expInfo b) {
	if (a.type == INT_T && b.type == INT_T) {
		res->type = INT_T;
		res->val.ival = a.val.ival * b.val.ival;
	    printf("(%d) %d * (%d) %d = (%d) %d\n", a.type, a.val.ival, b.type, b.val.ival, res->type, res->val.ival); //todo: remove
	} else {
		res->type = FLOAT_T;
		if (a.type == b.type == FLOAT_T)
			res->val.fval = a.val.fval * b.val.fval;
	    else if (a.type == FLOAT_T)
			res->val.fval = a.val.fval * b.val.ival;
	    else if (b.type == FLOAT_T)
			res->val.fval = a.val.ival * b.val.fval;
	}
}

void divide(struct s_expInfo* res, struct s_expInfo a, struct s_expInfo b) {
	if (a.type == INT_T && b.type == INT_T) {
		res->type = INT_T;
		res->val.ival = a.val.ival / b.val.ival;
	} else {
		res->type = FLOAT_T;
		if (a.type == b.type == FLOAT_T)
			res->val.fval = a.val.fval / b.val.fval;
	    else if (a.type == FLOAT_T)
			res->val.fval = a.val.fval / b.val.ival;
	    else if (b.type == FLOAT_T)
			res->val.fval = a.val.ival / b.val.fval;
	}
}

void performAddop(struct s_expInfo* res, 
                  struct s_expInfo a, 
                  enum e_addopType opType,
                  struct s_expInfo b) {
	switch (opType) {
		case PLUS: plus(res, a, b); break;
		case MINUS: minus(res, a, b); break;
	}
	
	if (res->type == INT_T) printf("Performed ADDLOP. res = %d\n", res->val.ival);
	else printf("Performed ADDLOP. res = %.1f\n", res->val.fval);
}

void plus(struct s_expInfo* res, struct s_expInfo a, struct s_expInfo b) {
	if (a.type == INT_T && b.type == INT_T) {
		res->type = INT_T;
		res->val.ival = a.val.ival + b.val.ival;
	    printf("(%d) %d + (%d) %d = (%d) %d\n", a.type, a.val.ival, b.type, b.val.ival, res->type, res->val.ival); //todo: remove
	} else {
		res->type = FLOAT_T;
		if (a.type == b.type == FLOAT_T)
			res->val.fval = a.val.fval + b.val.fval;
	    else if (a.type == FLOAT_T)
			res->val.fval = a.val.fval + b.val.ival;
	    else if (b.type == FLOAT_T)
			res->val.fval = a.val.ival + b.val.fval;
	}
}

void minus(struct s_expInfo* res, struct s_expInfo a, struct s_expInfo b) {
	if (a.type == INT_T && b.type == INT_T) {
		res->type = INT_T;
		res->val.ival = a.val.ival - b.val.ival;
	} else {
		res->type = FLOAT_T;
		if (a.type == b.type == FLOAT_T)
			res->val.fval = a.val.fval - b.val.fval;
	    else if (a.type == FLOAT_T)
			res->val.fval = a.val.fval - b.val.ival;
	    else if (b.type == FLOAT_T)
			res->val.fval = a.val.ival - b.val.fval;
	}
}

void assignValue(char name[MAX_LEN], struct s_expInfo exp) {
	// Find the symbol
	struct s_symbol* symbolEntry = symbolLookup(name);
	if (symbolEntry == NULL) {
		yyerror("id not declared");
		return;
	}
	printf("symbol %s found in table\n", symbolEntry->name);
	
	// Assign the value
	if (symbolEntry->type == FLOAT_T && exp.type == FLOAT_T)
		symbolEntry->val.fval = exp.val.fval;
	else if (symbolEntry->type == INT_T && exp.type == INT_T)
		symbolEntry->val.ival = exp.val.ival;
	else if (symbolEntry->type == FLOAT_T && exp.type == FLOAT_T)
		symbolEntry->val.fval = exp.val.fval;
	else if (symbolEntry->type == FLOAT_T && exp.type == INT_T)
		symbolEntry->val.fval = (double) exp.val.ival;
    else {
		yyerror("assigning FLOAT value to an INT variable\n");
		return;
	}
	
	if (symbolEntry->type == INT_T) printf("value assigned: %s=%d\n", symbolEntry->name, symbolEntry->val.ival);
	else printf("value assigned: %s=%.1f\n", symbolEntry->name, symbolEntry->val.fval);
}

struct s_symbol* symbolLookup(char name[MAX_LEN]) {
	struct s_symbolTableNode* currNode = g_symbolTableHead;
	while (currNode != NULL){
		if (strcmp(currNode->symbol.name, name) == 0)
			return &(currNode->symbol);
		currNode = currNode->next;
	}
	return NULL;
}

void variableToExpression(struct s_expInfo* dest, char varName[MAX_LEN]) {
	// Find the symbol
	struct s_symbol* symbol = symbolLookup(varName);
	if (symbol == NULL) {
		yyerror("id not declared");
		return;
	}
	
	// Set values to expression
	dest->type = symbol->type;
	switch (dest->type) {
		case INT_T: dest->val.ival = symbol->val.ival; break;
		case FLOAT_T: dest->val.fval = symbol->val.fval; break;
	}
}
