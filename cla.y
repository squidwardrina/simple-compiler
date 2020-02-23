%code {

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

#define IN_FILE_TYPE        ".ou"
#define OUT_FILE_TYPE       ".qud"
#define SIGNATURE           "\nRina Fridland\n"
#define COMMAND_LEN         100


extern int yylex (void);
void yyerror (const char *s);
extern FILE *yyout;

void createOutFileName(char*, char*);
void insertVarToTable(char[], enum e_varType);
struct s_idNode* createIdNode(char*, struct s_idNode*);
void insertVarsToTable(enum e_varType, struct s_idNode*);
void freeSymbolTable();
void saveGeneratedCode();
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
void numberToExpression(struct s_expInfo*, struct s_numInfo*);
void generateCommand(char[], char[]);
void newTempId(char[], enum e_varType);
void inputCommand(char[]);
void oututCommand(struct s_expInfo);

// Symbol table stuff
struct s_symbol {
	char name[MAX_LEN];
	enum e_varType type;
};
struct s_symbolTableNode {
	struct s_symbol symbol;
	struct s_symbolTableNode* next;
};
struct s_symbolTableNode* g_symbolTableHead = NULL; // using globals is wrong, but first things first

// Code generation stuff
struct s_generatedCommandNode {
	char command[COMMAND_LEN];
	char jumpFlagName[10];
	struct s_generatedCommandNode* next;
};
int g_generateCode = 1; // if error found, code shouldn't be generated
struct s_generatedCommandNode* g_generatedCommandsHead = NULL; // an array would be more efficient, but reallocs logic is buggy and time consuming to write

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
		char resVarName[MAX_LEN];
	};
	struct s_numInfo {
		enum e_varType type;
		union u_numval val;
	};
}

%union {
   struct s_numInfo numInfo;
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
%left <numInfo> NUM
%left <sval> ID

%type <varType> type
%type <idList> idlist
%type <expInfo> expression term factor

%error-verbose

%%

program     : declarations { printf("\n-- declarations ended --\n\n"); } stmt_block { freeSymbolTable(); saveGeneratedCode(); }

declarations: declarations declaration
            | /* empty */ 

declaration : idlist ':' type ';' { insertVarsToTable($3, $1.idlistHead); }

type        : INT                 { $$ = INT_T; }
            | FLOAT               { $$ = FLOAT_T; }

idlist      : idlist ',' ID       { $$.idlistHead = createIdNode($3, $1.idlistHead); }
            | ID                  { $$.idlistHead = createIdNode($1, NULL); }

stmt        : assignment_stmt
            | input_stmt
            | output_stmt
            | cast_stmt
            | if_stmt
            | while_stmt
            | switch_stmt
            | break_stmt
            | stmt_block

assignment_stmt : ID '=' expression ';'     { assignValue($1, $3); }

input_stmt  : INPUT '(' ID ')' ';'          { inputCommand($3); }

output_stmt : OUTPUT '(' expression ')' ';' { oututCommand($3); }

cast_stmt   : ID '=' STATIC_CAST '(' type ')' '(' expression ')' ';'

if_stmt     : IF '(' boolexpr ')' stmt ELSE stmt

while_stmt  : WHILE '(' boolexpr ')' stmt

switch_stmt : SWITCH '(' expression ')' '{' caselist DEFAULT ':' stmtlist '}'

caselist    : caselist CASE NUM ':' stmtlist
            | /* empty */

break_stmt  : BREAK ';'

stmt_block  : '{' stmtlist '}'

stmtlist    : stmtlist stmt
            | /* empty */

boolexpr    : boolexpr OR boolterm
            | boolterm

boolterm    : boolterm AND boolfactor
            | boolfactor

boolfactor  : NOT '(' boolexpr ')'
            | expression RELOP expression

expression  : expression ADDOP term        { performAddop(&($$), $1, $2, $3); }
            | term                         { copyExpInfo($$, $1); }

term        : term MULOP factor            { performMulop(&($$), $1, $2, $3); }
            | factor                       { copyExpInfo($$, $1); }

factor      : '(' expression ')'           { copyExpInfo($$, $2); }
            | ID                           { variableToExpression(&($$), $1); }
            | NUM                          { numberToExpression(&($$), &($1)); }

%%

int main (int argc, char **argv) {
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

    // Open output file
    char outFileName[255];
    createOutFileName(argv [1], outFileName);
    if ((yyout = fopen(outFileName, "w")) == NULL) {
        fprintf(stderr, "Error opening output file");
        exit(1);
    }

    yyparse ();

    fprintf(yyout, SIGNATURE);
    fclose(yyin);
    fclose(yyout);
    return 0;
}

void createOutFileName(char* inFileName, char* outFileName) {
   int filenameLen = strlen(inFileName) - strlen(IN_FILE_TYPE);
   memcpy(outFileName, inFileName, filenameLen);
   outFileName[filenameLen] = '\0';
   strcat(outFileName, OUT_FILE_TYPE);
}

void insertVarToTable(char name[MAX_LEN], enum e_varType varType) {
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
		insertVarToTable(currNode->name, varType);

		// Move to next node & free the memory for the prev one
		prevNode = currNode;
		currNode = currNode->next;
		free(prevNode);
	}
}

void freeSymbolTable() {
	struct s_symbolTableNode* currNode = g_symbolTableHead;
	struct s_symbolTableNode* prevNode = NULL;

	printf("\n---------------\nsymbol table at the end of program:");
	while (currNode != NULL) {
		// Print the symbol table before deleting
	    printf("\n\ttype #%d: %s", currNode->symbol.type, currNode->symbol.name);
		
		prevNode = currNode;
		currNode = currNode->next;
		free(prevNode);
	}
	printf("\nsymbol table freed\n");
}

void saveGeneratedCode() {
	struct s_generatedCommandNode* currNode = g_generatedCommandsHead;
	struct s_generatedCommandNode* prevNode = NULL;
	
	while (currNode != NULL) {
		fprintf(yyout, "%s\n", currNode->command);
		
		prevNode = currNode;
		currNode = currNode->next;
		free(prevNode);
	}
}

char prefixChar(enum e_varType type) {
	switch (type) {
		case INT_T: return 'I';
		case FLOAT_T: return 'R';
	}
}

void copyExpInfo(struct s_expInfo src, struct s_expInfo dest) {
	dest.type = src.type;
	strcpy(dest.resVarName, src.resVarName);
}

void performOp(struct s_expInfo* res,struct s_expInfo a,
			   char* opStr, struct s_expInfo b) {
	// Check the type of the result
	char command[COMMAND_LEN];
	char prefix;
	if (a.type == INT_T && b.type == INT_T) {
		res->type = INT_T;
		newTempId(res->resVarName, INT_T);
	    prefix = 'I';
	} else {
		res->type = FLOAT_T;
		newTempId(res->resVarName, FLOAT_T);	
	    prefix = 'R';
	}
	
	// Generate the command
	sprintf(command, "%c%s %s %s %s", prefix, opStr, res->resVarName, 
	                                  a.resVarName, b.resVarName);	
	generateCommand(command, "");
}

void performMulop(struct s_expInfo* res, 
                  struct s_expInfo a, 
                  enum e_mulopType opType,
                  struct s_expInfo b) {
	// Save command name to string
	char opStr[4];
	switch (opType) {
		case MUL: stpcpy(opStr, "MLT"); break;
		case DIV: stpcpy(opStr, "DIV"); break;
	}
	performOp(res, a, opStr, b);
}

void performAddop(struct s_expInfo* res, struct s_expInfo a, 
                  enum e_addopType opType, struct s_expInfo b) { // todo: merge e_addopType and e_mulopType
	// Save command name to string
	char opStr[4];
	switch (opType) {
		case MUL: stpcpy(opStr, "ADD"); break;
		case DIV: stpcpy(opStr, "SUB"); break;
	}
	performOp(res, a, opStr, b);
}

void assignValue(char name[MAX_LEN], struct s_expInfo exp) {
	// Find the symbol
	struct s_symbol* symbolEntry = symbolLookup(name);
	if (symbolEntry == NULL) {
		yyerror("id not declared");
		return;
	}
	printf("symbol %s found in table\n", symbolEntry->name);

	char commandName[5];
	char command[COMMAND_LEN];
	
	if (symbolEntry->type == FLOAT_T && exp.type == FLOAT_T)  // float <- float
		strcpy(commandName, "RASN");
	else if (symbolEntry->type == INT_T && exp.type == INT_T)  // int <- int
		strcpy(commandName,"IASN");
	else if (symbolEntry->type == FLOAT_T && exp.type == INT_T)  // float <- int
		strcpy(commandName,"ITOR");
    else {
		yyerror("assigning FLOAT value to an INT variable\n");
		return;
	}
	sprintf(command, "%s %s %s", commandName, symbolEntry->name, exp.resVarName);
	generateCommand(command, "");
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
	strcpy(dest->resVarName, varName);
}

void numberToExpression(struct s_expInfo* dest, struct s_numInfo* numInfo) {
	// Create temp var for the expression
	char varName[MAX_LEN];
	newTempId(varName, numInfo->type);

	// Set values to expression
	dest->type = numInfo->type;
	strcpy(dest->resVarName, varName);

	// Assign the number to the new var
	char command[COMMAND_LEN];
	if (dest->type == INT_T)
		sprintf(command, "IASN %s %d", varName, numInfo->val.ival);	
	else 
		sprintf(command, "RASN %s %f", varName, numInfo->val.fval);	
	generateCommand(command, "");
}

void newTempId(char name[MAX_LEN], enum e_varType type) {
	// Create new var name
	static int id = 1;
	sprintf(name, "t%d", id);
	insertVarToTable(name, type);
	id++;
}

void generateCommand(char command[COMMAND_LEN], char jumpFlagName[10]) {
	static struct s_generatedCommandNode* lastNode = NULL; // pointer to the last command
	
	// Don't generate code if flag is off
	if (!g_generateCode) 
		return;
	
	// Create the new command node
	struct s_generatedCommandNode* newNode = 
		(struct s_generatedCommandNode*) malloc(sizeof(struct s_generatedCommandNode));
	strcpy(newNode->command, command);
	strcpy(newNode->jumpFlagName, jumpFlagName);
	newNode->next = NULL;

    // Add to commands list
	if (g_generatedCommandsHead == NULL) 
		g_generatedCommandsHead = newNode; // add as first command
	else 
		lastNode->next = newNode; // add after latest command
	
	lastNode = newNode; // this is the last command now
	
	printf("-------------------------------------------------> command generated: %s", newNode->command);
	if(jumpFlagName[0] != '\0')
		printf(", jumpFlagName: %s", newNode->jumpFlagName);
	printf("\n");
}

void inputCommand(char varName[MAX_LEN]) {
	// Find the symbol
	struct s_symbol* symbol = symbolLookup(varName);
	if (symbol == NULL) {
		yyerror("id not declared");
		return;
	}
	
	// Generate command
	char command[COMMAND_LEN];
	sprintf(command, "%cINP %s", prefixChar(symbol->type), varName);
	generateCommand(command, "");
}

void oututCommand(struct s_expInfo exp) {
	char command[COMMAND_LEN];
	sprintf(command, "%cPRT %s", prefixChar(exp.type), exp.resVarName);
	generateCommand(command, "");
}
