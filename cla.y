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

struct s_idNode* createIdNode(char*, struct s_idNode*);
void insertVarsToTable(enum e_varType, struct s_idNode*);
void generateCommand(char[], char[]);

void copyExpInfo(struct s_expInfo, struct s_expInfo);
void varToExp(struct s_expInfo*, char[]);
void numToExp(struct s_expInfo*, struct s_numInfo*);

void addopCommand(struct s_expInfo*, struct s_expInfo, enum e_addopType,struct s_expInfo);
void mulopCommand(struct s_expInfo*, struct s_expInfo, enum e_mulopType,struct s_expInfo);
void assignCommand(char[], struct s_expInfo);
void inputCommand(char[]);
void oututCommand(struct s_expInfo);
void castCommand(char[], enum e_varType, struct s_expInfo);
void relopCommand(char[], struct s_expInfo, enum e_relopType, struct s_expInfo);

void freeSymbolTable();
void freeAndSaveGeneratedCode(int, char*);

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

enum e_bool {FALSE, TRUE};

}

%code requires {
	#define MAX_LEN 10

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
%type <sval> boolfactor boolterm boolexpr

%error-verbose

%%

program     : declarations stmt_block { generateCommand("HALT", ""); }

declarations: declarations declaration
            | /* empty */ 

declaration : idlist ':' type ';' { insertVarsToTable($3, $1.idlistHead); }
			| error ';'

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
			| error ';'

assignment_stmt : ID '=' expression ';'     { assignCommand($1, $3); }

input_stmt  : INPUT '(' ID ')' ';'          { inputCommand($3); }

output_stmt : OUTPUT '(' expression ')' ';' { oututCommand($3); }

cast_stmt   : ID '=' STATIC_CAST '(' type ')' '(' expression ')' ';' { castCommand($1, $5, $8); }

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

boolfactor  : NOT '(' boolexpr ')'         { strcpy($$, $3); } //todo: NOT
            | expression RELOP expression  { relopCommand($$, $1, $2, $3); }

expression  : expression ADDOP term        { addopCommand(&($$), $1, $2, $3); }
            | term                         { copyExpInfo($$, $1); }

term        : term MULOP factor            { mulopCommand(&($$), $1, $2, $3); }
            | factor                       { copyExpInfo($$, $1); }

factor      : '(' expression ')'           { copyExpInfo($$, $2); }
            | ID                           { varToExp(&($$), $1); }
            | NUM                          { numToExp(&($$), &($1)); }

%%

void createOutFileName(char* inFileName, char* outFileName) {
   int filenameLen = strlen(inFileName) - strlen(IN_FILE_TYPE);
   memcpy(outFileName, inFileName, filenameLen);
   outFileName[filenameLen] = '\0';
   strcat(outFileName, OUT_FILE_TYPE);
}

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

    yyparse();

	// Free the memory and flush the code to file if needed
	freeSymbolTable();
	char outFileName[255];
	createOutFileName(argv[1], outFileName);
	freeAndSaveGeneratedCode(g_generateCode, outFileName);

    fclose(yyin);
    return 0;
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

void yyerror(const char *s) {
	extern int line;
	fprintf (stderr, "line %d: %s\n", line, s);
	g_generateCode = 0; // stop generating code
}

void newTempId(char* nameBuff, enum e_varType type) {
	static int id = 1;

	sprintf(nameBuff, "t_%d", id); // guaranteed no collision, because '_' forbidden in source language
	insertVarToTable(nameBuff, type);
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

void freeAndSaveGeneratedCode(int flushCodeToFile, char* outFileName) {
	// Open output file if needed
	if (flushCodeToFile) {
		if ((yyout = fopen(outFileName, "w")) == NULL) {
			fprintf(stderr, "Error opening output file");
			exit(1);
		}
	}
	
	// Free the memory and flush to file if needed
	struct s_generatedCommandNode* currNode = g_generatedCommandsHead;
	struct s_generatedCommandNode* prevNode = NULL;
	while (currNode != NULL) {
		if (flushCodeToFile) 
			fprintf(yyout, "%s\n", currNode->command);
		
		prevNode = currNode;
		currNode = currNode->next;
		free(prevNode);
	}	
	
	// Close the file if needed
	if (flushCodeToFile) {
		fprintf(yyout, SIGNATURE);
		fclose(yyout);
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

void anyopCommand(struct s_expInfo* res,struct s_expInfo a,
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

void mulopCommand(struct s_expInfo* res, 
                  struct s_expInfo a, 
                  enum e_mulopType opType,
                  struct s_expInfo b) {
	// Save command name to string
	char opStr[4];
	switch (opType) {
		case MUL: stpcpy(opStr, "MLT"); break;
		case DIV: stpcpy(opStr, "DIV"); break;
	}
	anyopCommand(res, a, opStr, b);
}

void addopCommand(struct s_expInfo* res, struct s_expInfo a, 
                  enum e_addopType opType, struct s_expInfo b) { // todo: merge e_addopType and e_mulopType
	// Save command name to string
	char opStr[4];
	switch (opType) {
		case MUL: stpcpy(opStr, "ADD"); break;
		case DIV: stpcpy(opStr, "SUB"); break;
	}
	anyopCommand(res, a, opStr, b);
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

struct s_symbol* symbolLookupWithAssert(char name[MAX_LEN]) {
	struct s_symbol* symbol = symbolLookup(name);
	if (symbol == NULL)
		yyerror("id not declared");
	return symbol;
}

void assignCommand(char name[MAX_LEN], struct s_expInfo exp) {
	struct s_symbol* symbol = symbolLookupWithAssert(name);
	
	char commandName[5];
	char command[COMMAND_LEN];
	
	if (symbol->type == FLOAT_T && exp.type == FLOAT_T)  // float <- float
		strcpy(commandName, "RASN");
	else if (symbol->type == INT_T && exp.type == INT_T)  // int <- int
		strcpy(commandName,"IASN");
	else if (symbol->type == FLOAT_T && exp.type == INT_T)  // float <- int
		strcpy(commandName,"ITOR");
    else {
		yyerror("assigning FLOAT value to an INT variable\n");
		return;
	}
	sprintf(command, "%s %s %s", commandName, symbol->name, exp.resVarName);
	generateCommand(command, "");
}

void varToExp(struct s_expInfo* dest, char varName[MAX_LEN]) {
	struct s_symbol* symbol = symbolLookupWithAssert(varName);

	// Set values to expression
	dest->type = symbol->type;
	strcpy(dest->resVarName, varName);
}

void numToExp(struct s_expInfo* dest, struct s_numInfo* numInfo) {
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

void inputCommand(char varName[MAX_LEN]) {
	struct s_symbol* symbol = symbolLookupWithAssert(varName);
	
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

void castCommand(char varName[MAX_LEN], enum e_varType toType, struct s_expInfo exp) {
	struct s_symbol* symbol = symbolLookupWithAssert(varName);
    if (symbol->type != toType) {
		yyerror("casting type differs from variable type");
		return;
	}
	
	char commandName[5];
	if (toType == INT_T && exp.type == INT_T) strcpy(commandName, "IASN");
	else if (toType == INT_T && exp.type == FLOAT_T) strcpy(commandName, "RTOI");
	else if (toType == FLOAT_T && exp.type == INT_T) strcpy(commandName, "ITOR");
	else strcpy(commandName, "RASN");
	
	char command[COMMAND_LEN];
	sprintf(command, "%s %s %s", commandName, varName, exp.resVarName);
	generateCommand(command, "");	
}

void castToFloat(struct s_expInfo* exp) {
	// Create temp var for the expression
	char varName[MAX_LEN];
	newTempId(varName, FLOAT_T);

	// Generate cast command
	char command[COMMAND_LEN];
	sprintf(command, "ITOR %s %s", varName, exp->resVarName);
	generateCommand(command, "");

	// Set the new casted var name to exp
	strcpy(exp->resVarName, varName);
}

void relopCommand(char dest[MAX_LEN], struct s_expInfo exp1, 
                  enum e_relopType relop, struct s_expInfo exp2) {
	// Make sure types are equal
	if (exp1.type != exp2.type) {
		if (exp1.type == INT_T) castToFloat(&exp1);
		else castToFloat(&exp2);
	}
	char prefix = prefixChar(exp1.type);

	char cmdName[4];
	switch (relop) {
		case EQ: strcpy(cmdName, "EQL"); break;
		case NE: strcpy(cmdName, "NQL"); break;
		case GE:
		case GT: strcpy(cmdName, "GRT"); break;
		case LE:
		case LT: strcpy(cmdName, "LSS"); break;
	}

	newTempId(dest, INT_T);

	char command[COMMAND_LEN];
	sprintf(command, "%c%s %s %s %s", prefix, cmdName, dest, 
	                                  exp1.resVarName, exp2.resVarName);
	generateCommand(command, "");

	// In case of ">=" or "<=" add another condition with OR
	if (relop == GE || relop == LE) {
		// Generate the "==" command
		char helpVar[MAX_LEN];
		newTempId(helpVar, INT_T);
		sprintf(command, "%cEQL %s %s %s", prefix, helpVar,
	                                       exp1.resVarName, exp2.resVarName);
		generateCommand(command, "");

		// Add the two results and check if we had at least one TRUE
		sprintf(command, "IADD %s %s %s", helpVar, dest, helpVar);
		generateCommand(command, "");
		sprintf(command, "IGRT %s %s 0", dest, helpVar);
		generateCommand(command, "");
	}
}