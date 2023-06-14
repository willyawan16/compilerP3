%{
#define ST_LENGTH 10
#define MAX_LINE_LENGTH 256
#define NUMBER_OF_ST 100
#define MAX_NUMBER_OF_PARAM 100
#define NO_EXT 99999
#define FUNCPARAM 99998
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
#include<stdbool.h>
#include<math.h>
#define Trace(t)        printf(t)
int yylex(void);
int yyerror(char *s);

extern FILE *yyin;
extern int yylineno;
extern char *yytext;

FILE* outFile;



union Value {
    int     none;
    int     integer;
    double  real;
    char*   string;
    bool    boolean;
};

struct MultiValue {
    int type; // 0: none 1: int 2: real 3: string 4: bool
    bool hasValue;
    union Value value;
};

enum ValueType { // or DECLARATION TYPE
    VALNONE,
	VALINT,
	VALREAL,
	VALSTR,
	VALBOOL,
    VALARRAYREF,
};

enum StoreType {
    NONE,
	CONSTANT,
	VARIABLE,
    MULTIVAR, // ARRAY
    FUNC,
    PROC
};

struct ArrayAtr {
    int startIndex;
    int capacity;
};

struct FuncAtr {
    int size;
    int paramType[MAX_NUMBER_OF_PARAM];
    struct ArrayAtr arrayAtr[MAX_NUMBER_OF_PARAM];
};

struct SymbolTable {
    int index;
    char** strList;
	enum ValueType* valueType;
	enum StoreType* storeType;
    union Value* value;
    int* labels;
    struct ArrayAtr* arrayAtr;
    struct FuncAtr* funcAtr;
    size_t capacity;
    size_t size;
    struct SymbolTable* parent;
};


struct TermStack {
    struct SymbolTable* chosenTableStack;
    int foundIndex;
};

struct FuncCallingStack {
    bool funcCalling;
    int atParentIndex;
    int currentParamIndex;
};

struct SymbolTable** Col_ST;
int currentSize;
int currentTable;

struct TermStack termStack[10];
int topTerm;

struct FuncCallingStack funcCallingStack[10];
int topFuncCalling;

struct SymbolTable symbols;

void create();
void createChildTable();
int lookup(char* s);
int insert(char* s);
void dump();
int lookupInTable(char* s, struct SymbolTable* parentTable);
void printSymbols();
void printChosenSymbols(int index);
void returnToParent();
void initFlags();
void resetFunctionGrammarFlags();
enum ValueType valTypeBuffer = VALINT;
enum ValueType constExpTypeBuffer = VALINT;

void transGlobalVariable(char* varName, struct MultiValue data);
void transLocalVariable(char* varName, struct MultiValue data);
void transExprTerm(struct MultiValue data);
void printSymbolInformationIn();
void printSymbolInformationOut();
void outTabs();
void printLine(int lineNum, char* lineText);
char* getLineText();

// Flags
bool funcScope;
bool procScope;
bool needConstExp;
bool funcDeclaration;
bool funcCalling;
int currentParamIndex;
struct SymbolTable* functionParent;
int atParentIndex;
int foundIndex;
int scopeReturnType;
bool isCallingArray;

int currentLabel;
int labelCounter;

int lCounter;
int lStack[100];
int lStackPointer;

bool isWritingLocalVar;
bool isWritingStmt;
char* filename;
int tabs;

int forVarLabel[20];
int forVarIndex[20];
int forVarPointer;
bool isForDecr[20];
int forDecrPointer;

bool inMain;
%}

%union {
    int     ival;
    double  dval;
    char*   sval;
    bool    bval;
    struct {
        int type;
        union {
            int     uint;
            double  ureal;
            char*   ustr;
            bool    ubool;
            struct {
                int arrayType;
                int startIndex;
                int capacity;
            } uarray;
        } value;
    } multival;
};

/* tokens */
%token ARRAY BEGINT CHAR BOOL CONST DECREASING DEFAULT DO ELSE END EXIT FOR FUNCTION GET IF LOOP OF PUT PROCEDURE RESULT RETURN SKIP THEN VAR WHEN
%token RELOPL RELOPLT RELOPG RELOPGT RELOPEQ AND OR NOT
%token DOT COMMA COLON SEMICOLON POPEN PCLOSE SOPEN SCLOSE BOPEN BCLOSE
%token ADD SUB MUL DIV MOD
%token ASSIGN
%token NEWLINE

%token <sval> STRING
%token <ival> INT ID
%token <dval> REAL
%token <bval> TRUE FALSE

%type <bval> bool_exp
%type <multival> exp
%type <ival> id_ext


%left OR
%left AND
%right NOT
%left RELOPLT RELOPLE RELOPGT RELOPGE RELOPEQ RELOPNE
%left ADD SUB
%left MUL DIV MOD
%nonassoc UMINUS

%start program
%%

/* ----------- Main Structure ----------- */
program: {
        fprintf(outFile, "class %s\n{\n", filename);
        tabs++;
    } decls {
        fprintf(outFile, "\tmethod public static void main(java.lang.String[])\n");
        fprintf(outFile, "\tmax_stack 15\n");
        fprintf(outFile, "\tmax_locals 15\n");
        fprintf(outFile, "\t{\n");
        tabs++;
        inMain = true;
        isWritingLocalVar = true;
    } stmts {
        fprintf(outFile, "\t\treturn\n");
        fprintf(outFile, "\t}\n");
        fprintf(outFile, "}\n");
    }
    ;
    
// newline: /* empty */ | NEWLINE

decls: /* empty */
    | decl decls 
    ;

stmts: /* empty */ 
    | stmt stmts 
    ;
/* ----------- Main Structure ----------- */

// const_exp: INT          { 
//                             constExpTypeBuffer = VALINT;
//                             $<ival>$ = $1;
//                         }
//     | REAL              { 
//                             constExpTypeBuffer = VALREAL;
//                             $<dval>$ = $1;
//                         }
//     | STRING            { 
//                             constExpTypeBuffer = VALSTR;
//                             $<sval>$ = $1;
//                         }
//     | TRUE              { 
//                             constExpTypeBuffer = VALBOOL;
//                             $<bval>$ = $1;
//                         }
//     | FALSE             { 
//                             constExpTypeBuffer = VALBOOL;
//                             $<bval>$ = $1;
//                         }
//     ;

/* Value Type Non-Terminal */
val_type: INT           { 
                            valTypeBuffer = VALINT;
                        }
    | REAL              { 
                            valTypeBuffer = VALREAL;
                        }
    | STRING            { 
                            valTypeBuffer = VALSTR;
                        }
    | BOOL              { 
                            valTypeBuffer = VALBOOL;
                        }
    ;

/* Declaration is divided into variables and functions declaration */
decl: var_decl | func_decl

/* Variable Declaration */
var_decl: 
    /* const id := const_exp */
    CONST ID ASSIGN                             {needConstExp = true; } 
    exp                                         { 
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 

                                                    Col_ST[currentTable]->storeType[$2] = CONSTANT;
                                                    // Col_ST[currentTable]->valueType[$2] = constExpTypeBuffer;
                                                    switch($<multival.type>5) {
                                                        case (int)VALINT:
                                                            Col_ST[currentTable]->valueType[$2] = VALINT;
                                                            Col_ST[currentTable]->value[$2].integer = $<multival.value.uint>5;
                                                            break;
                                                        case (int)VALREAL:
                                                            Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            Col_ST[currentTable]->value[$2].real = $<multival.value.ureal>5;
                                                            break;
                                                        case (int)VALSTR:
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            Col_ST[currentTable]->value[$2].string = $<multival.value.ustr>5;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            Col_ST[currentTable]->value[$2].boolean = $<multival.value.ubool>5;
                                                            break;
                                                    }

                                                    // printSymbols();
                                                    needConstExp = false;
                                                } 
    /* const id :val_type := const_exp */
    | CONST ID COLON val_type ASSIGN            {needConstExp = true;}
    exp                                         { 
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = CONSTANT;
                                                    
                                                    switch($<multival.type>7) {
                                                        case (int)VALINT:
                                                            if(valTypeBuffer != VALINT) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALINT;
                                                            Col_ST[currentTable]->value[$2].integer = $<multival.value.uint>7;
                                                            break;
                                                        case (int)VALREAL:
                                                            if(valTypeBuffer != VALREAL) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            Col_ST[currentTable]->value[$2].real = $<multival.value.ureal>7;
                                                            break;
                                                        case (int)VALSTR:
                                                            if(valTypeBuffer != VALSTR) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            Col_ST[currentTable]->value[$2].string = $<multival.value.ustr>7;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            if(valTypeBuffer != VALBOOL) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            Col_ST[currentTable]->value[$2].boolean = $<multival.value.ubool>7;
                                                            break;
                                                    }

                                                    //  printSymbols();
                                                    needConstExp = false;
                                                }
    /* var id := const_exp */
    | VAR ID ASSIGN                             {
                                                    needConstExp = true;
                                                    if(currentTable == 0) isWritingLocalVar = false;
                                                    else isWritingLocalVar = true;
                                                } 
    exp                                         {
                                                    struct MultiValue data;
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                    data.hasValue = true;

                                                    switch($<multival.type>5) {
                                                        case (int)VALINT:
                                                            Col_ST[currentTable]->valueType[$2] = VALINT;
                                                            data.type = (int)VALINT;
                                                            data.value.integer = $<multival.value.uint>5;
                                                            break;
                                                        case (int)VALREAL:
                                                            Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            data.type = (int)VALREAL;
                                                            data.value.real = $<multival.value.ureal>5;
                                                            break;
                                                        case (int)VALSTR:
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            data.type = (int)VALSTR;
                                                            data.value.string = $<multival.value.ustr>5;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            data.type = (int)VALBOOL;
                                                            data.value.boolean = $<multival.value.ubool>5;
                                                            break;
                                                    }
                                                    // TODO: store value
                                                    if(currentTable == 0) {
                                                        // global
                                                        transGlobalVariable(Col_ST[currentTable]->strList[$2], data);
                                                    } else {
                                                        // local
                                                        Col_ST[currentTable]->labels[$2] = currentLabel;
                                                        transLocalVariable(Col_ST[currentTable]->strList[$2], data);
                                                    }

                                                    // printSymbols();
                                                    needConstExp = false;
                                                    if(!inMain)
                                                        isWritingLocalVar = false;
                                                }
    /* var id :val_type := const_exp */
    | VAR ID COLON val_type ASSIGN              {
                                                    needConstExp = true; 
                                                    if(currentTable == 0) isWritingLocalVar = false;
                                                    else isWritingLocalVar = true;
                                                }
    exp                                         {              
                                                    struct MultiValue data;                                      
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                    data.hasValue = true;

                                                    switch($<multival.type>7) {
                                                        case (int)VALINT:
                                                            if(valTypeBuffer != VALINT) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALINT;
                                                            data.type = (int)VALINT;
                                                            data.value.integer = $<multival.value.uint>7;
                                                            break;
                                                        case (int)VALREAL:
                                                            if(valTypeBuffer != VALREAL) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALREAL;
                                                            data.type = (int)VALREAL;
                                                            data.value.real = $<multival.value.ureal>7;
                                                            break;
                                                        case (int)VALSTR:
                                                            if(valTypeBuffer != VALSTR) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALSTR;
                                                            data.type = (int)VALSTR;
                                                            data.value.string = $<multival.value.ustr>7;
                                                            break;  
                                                        case (int)VALBOOL:
                                                            if(valTypeBuffer != VALBOOL) {
                                                                return yyerror("Error: Different value type");
                                                            } 
                                                            Col_ST[currentTable]->valueType[$2] = VALBOOL;
                                                            data.type = (int)VALBOOL;
                                                            data.value.boolean = $<multival.value.ubool>7;
                                                            break;
                                                    }
                                                    // TODO: store value
                                                    if(currentTable == 0) {
                                                        // global
                                                        transGlobalVariable(Col_ST[currentTable]->strList[$2], data);
                                                    } else {
                                                        // local
                                                        Col_ST[currentTable]->labels[$2] = currentLabel;
                                                        transLocalVariable(Col_ST[currentTable]->strList[$2], data);
                                                    }

                                                    // printSymbols();
                                                    needConstExp = false;
                                                    if(!inMain)
                                                        isWritingLocalVar = false;
                                                }
    /* var id :val_type */
    | VAR ID COLON val_type                     {
                                                    struct MultiValue data;     
                                                    if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                        return yyerror("Error: Identifier existed");
                                                    } 
                                                    
                                                    Col_ST[currentTable]->storeType[$2] = VARIABLE;
                                                    Col_ST[currentTable]->valueType[$2] = valTypeBuffer;
                                                    
                                                    data.hasValue = false;
                                                    data.type = (int)valTypeBuffer;

                                                    if(currentTable == 0) {
                                                        // global
                                                        transGlobalVariable(Col_ST[currentTable]->strList[$2], data);
                                                    } else {
                                                        // local
                                                        Col_ST[currentTable]->labels[$2] = currentLabel;
                                                        transLocalVariable(Col_ST[currentTable]->strList[$2], data);
                                                    }

                                                    // printSymbols();
                                                }
    /* var id :array int..int of val_type */
    /* ARRAY WONT BE USE */
    // | VAR ID COLON ARRAY INT DOT DOT INT OF val_type    {
    //                                                         if(Col_ST[currentTable]->storeType[$2] != NONE) {
    //                                                             return yyerror("Error: Identifier existed");
    //                                                         } 
                                                            
    //                                                         Col_ST[currentTable]->storeType[$2] = MULTIVAR;
    //                                                         Col_ST[currentTable]->valueType[$2] = valTypeBuffer;
                                                            
    //                                                         if($5 != 0 && $5 != 1) {
    //                                                             return yyerror("Error: Start index should be either 0 or 1");
    //                                                         }

    //                                                         if($5 > $8) {
    //                                                             return yyerror("Error: Invalid index range");
    //                                                         }

    //                                                         Col_ST[currentTable]->arrayAtr[$2].startIndex = $5;
    //                                                         Col_ST[currentTable]->arrayAtr[$2].capacity = $8 - $5 + 1;
    //                                                         // TODO: store value
    //                                                         printSymbols();
    //                                                     }
    ;

/* Simultanoues variable declaration */
// var_decls: /* empty */ 
//     | var_decl var_decls
//     ;

/* Statement Body */
stmt_body: /* empty */ | var_decl stmt_body | stmt stmt_body;
// ---old---
// stmt_body: var_decl stmts;


/* For Function argument declaration */
func_args: POPEN formal_args PCLOSE 
    ;
formal_args: /*empty*/
    | formal_arg nextFormal_args
    ;
nextFormal_args: /* empty */
    | COMMA formal_args
    ;
formal_arg: ID COLON val_type   { // formal argument usual form
                                    if(Col_ST[currentTable]->storeType[$1] != NONE) {
                                        return yyerror("Error: Identifier existed");
                                    } 
                                    
                                    Col_ST[currentTable]->storeType[$1] = VARIABLE;
                                    Col_ST[currentTable]->valueType[$1] = valTypeBuffer;
                                    functionParent->funcAtr[atParentIndex].paramType[currentParamIndex] = (int)valTypeBuffer;
                                    currentParamIndex++;

                                    struct MultiValue data;
                                    data.type = (int)valTypeBuffer;
                                    data.hasValue = false;
                                    Col_ST[currentTable]->labels[$1] = currentLabel;
                                    transLocalVariable(Col_ST[currentTable]->strList[$1], data);
                                }
    | ID COLON ARRAY INT DOT DOT INT OF val_type        { // formal argument array form
                                                            if(Col_ST[currentTable]->storeType[$1] != NONE) {
                                                                return yyerror("Error: Identifier existed");
                                                            } 
                                                            
                                                            Col_ST[currentTable]->storeType[$1] = MULTIVAR;
                                                            Col_ST[currentTable]->valueType[$1] = valTypeBuffer;
                                                            Col_ST[currentTable]->arrayAtr[$1].startIndex = $4;
                                                            Col_ST[currentTable]->arrayAtr[$1].capacity = $7 - $4 + 1;

                                                            functionParent->funcAtr[atParentIndex].paramType[currentParamIndex] = (int)valTypeBuffer;
                                                            functionParent->funcAtr[atParentIndex].arrayAtr[currentParamIndex].startIndex = $4;
                                                            functionParent->funcAtr[atParentIndex].arrayAtr[currentParamIndex].capacity = $7 - $4 + 1;
                                                            currentParamIndex++;
                                                        }
    ;
/* --------------------------------- */

/* Function or Procedure declaration */
func_decl: 
    /* FUNCTION Declaration */
    FUNCTION ID     { 
                        currentLabel = 0;
                        labelCounter = 0;
                        printSymbolInformationIn();
                        funcScope = true;
                        funcDeclaration = true;
                        functionParent = Col_ST[currentTable];
                        atParentIndex = $2;
                        if(Col_ST[currentTable]->storeType[$2] != NONE) {
                            return yyerror("Error: Identifier existed");
                        }
                            
                        Col_ST[currentTable]->storeType[$2] = FUNC;
                        
                        createChildTable();
                        
                        printSymbols();
                        isWritingLocalVar = true;
                    } 
    func_args                               { printSymbols(); functionParent->funcAtr[atParentIndex].size = currentParamIndex;} 
    COLON val_type                          { 
                                                scopeReturnType = (int)valTypeBuffer;

                                                outTabs();
                                                fprintf(outFile, "method public static ");
                                                switch((int)valTypeBuffer) {
                                                    case VALINT:
                                                        fprintf(outFile, " int ");
                                                        break;
                                                    case VALBOOL:
                                                        fprintf(outFile, " boolean ");
                                                        break;
                                                }
                                                fprintf(outFile, "%s(", functionParent->strList[$2]);
                                                for(int i = 0; i < functionParent->funcAtr[atParentIndex].size; i++) {
                                                    if(i != 0) {
                                                        fprintf(outFile, ", ");
                                                    }
                                                    switch(functionParent->funcAtr[atParentIndex].paramType[i]) {
                                                        case VALINT:
                                                            fprintf(outFile, "int");
                                                            break;
                                                        case VALBOOL:
                                                            fprintf(outFile, "boolean");
                                                            break;
                                                    }
                                                }
                                                fprintf(outFile, ")\n");
                                                outTabs();
                                                fprintf(outFile, "max_stack 15\n");
                                                outTabs();
                                                fprintf(outFile, "max_locals 15\n");
                                                outTabs();
                                                fprintf(outFile, "{\n");
                                                tabs++;
                                            } 
    stmt_body END                           {
                                                tabs--;
                                                outTabs();
                                                fprintf(outFile, "}\n");
                                                printSymbolInformationOut();
                                                returnToParent();
                                                
                                                // assign return type
                                                Col_ST[currentTable]->valueType[$2] = valTypeBuffer;
                                                printSymbols();
                                            } 
    ID                                      {
                                                if($2 != $12) {
                                                    return yyerror("Error: Function closure does not match the function name");
                                                }

                                                // Reset
                                                isWritingLocalVar = false;
                                                resetFunctionGrammarFlags();
                                                currentLabel = 0;
                                                labelCounter = 0;
                                            }
    /* PROCEDURE Declaration */
    | PROCEDURE ID                          { 
                                                currentLabel = 0;
                                                labelCounter = 0;
                                                printSymbolInformationIn();
                                                procScope = true;
                                                scopeReturnType = -1;
                                                funcDeclaration = true;
                                                functionParent = Col_ST[currentTable];
                                                atParentIndex = $2;
                                                if(Col_ST[currentTable]->storeType[$2] != NONE) {
                                                    return yyerror("Error: Identifier existed");
                                                }
                                                    
                                                Col_ST[currentTable]->storeType[$2] = PROC;
                                                Col_ST[currentTable]->valueType[$2] = VALNONE;
                                                
                                                createChildTable();
                                                printSymbols();
                                                isWritingLocalVar = true;
                                            } 
    func_args                               { 
                                                printSymbols(); functionParent->funcAtr[atParentIndex].size = currentParamIndex; 

                                                outTabs();
                                                fprintf(outFile, "method public static void ");
                                                fprintf(outFile, "%s(", functionParent->strList[$2]);
                                                for(int i = 0; i < functionParent->funcAtr[atParentIndex].size; i++) {
                                                    if(i != 0) {
                                                        fprintf(outFile, ", ");
                                                    }
                                                    switch(functionParent->funcAtr[atParentIndex].paramType[i]) {
                                                        case VALINT:
                                                            fprintf(outFile, "int");
                                                            break;
                                                        case VALBOOL:
                                                            fprintf(outFile, "boolean");
                                                            break;
                                                    }
                                                }
                                                fprintf(outFile, ")\n");
                                                outTabs();
                                                fprintf(outFile, "max_stack 15\n");
                                                outTabs();
                                                fprintf(outFile, "max_locals 15\n");
                                                outTabs();
                                                fprintf(outFile, "{\n");
                                                tabs++;
                                            }  
    stmt_body END                           {
                                                tabs--;
                                                outTabs();
                                                fprintf(outFile, "}\n");
                                                printSymbolInformationOut();
                                                returnToParent();
                                                printSymbols();
                                            } 
    ID                                      {
                                                if($2 != $9) {
                                                    return yyerror("Error: Procedure closure does not match the function name");
                                                }
                                                // Reset
                                                isWritingLocalVar = false;
                                                resetFunctionGrammarFlags();
                                                currentLabel = 0;
                                                labelCounter = 0;
                                            }
    ;

/* Statements */
stmt: block
    | simple
    | proc_invoke
    | if_stmt
    | loop_stmt
    ;

/* Blocks */
block: BEGINT                   {
                                    createChildTable();
                                    printSymbolInformationIn();
                                    // printSymbols();
                                } 
    stmt_body END               {
                                    printSymbolInformationOut();
                                    returnToParent(); 
                                    // printSymbols();
                                }
    ;

/* Simple Statement */
simple: 
    // ID array_ref ASSIGN exp
    ID ASSIGN {isWritingLocalVar = true; }
    exp         { 

                                        struct SymbolTable* chosenTable = NULL;
                                        struct SymbolTable* parentTable = NULL;
                                        int foundIndex = -1;
                                        
                                        // Check whether is initialized
                                        if(Col_ST[currentTable]->storeType[$1] == NONE) {
                                            
                                            // Search in its ancestor symbol table
                                            bool found = false;
                                            parentTable = Col_ST[currentTable]->parent;
                                            while (!found) {
                                                if(parentTable == NULL) {
                                                    return yyerror("Error: Identifier is not assigned yet anywhere");
                                                }
                                                foundIndex = lookupInTable(Col_ST[currentTable]->strList[$1], parentTable);
                                                if(foundIndex != -1 && parentTable->storeType[foundIndex] != NONE) {
                                                    found = true;
                                                    break;
                                                }

                                                parentTable = parentTable->parent;
                                            }
                                            printf("FOUND in parent %s (table %d, index %d)! for assignment\n", Col_ST[currentTable]->strList[$1],parentTable->index, foundIndex );
                                            
                                            // CHOSEN TABLE TO PARENT TABLE
                                            chosenTable = parentTable;
                                        }

                                        if(parentTable == NULL) {
                                            // JUST FOR CURRENT TABLE VARIABLE
                                            chosenTable = Col_ST[currentTable];
                                            foundIndex = $1;
                                        }

                                        if(chosenTable->storeType[foundIndex] == CONSTANT) {
                                            return yyerror("Error: Cannot assign to constant variables");
                                        }

                                        if(chosenTable->storeType[foundIndex] == FUNC) {
                                            return yyerror("Error: Cannot assign to functions");
                                        }

                                        if(chosenTable->storeType[foundIndex] == PROC) {
                                            return yyerror("Error: Cannot assign to procedures");
                                        }

                                        // TODO: Do assignment to normal variable
                                        // printf("%d >< %d\n", (int)chosenTable->valueType[foundIndex], $<multival.type>4);
                                        if((int)chosenTable->valueType[foundIndex] == $<multival.type>4) {
                                            if(chosenTable->index == 0) {
                                                outTabs(); 
                                                fprintf(outFile, "putstatic ");
                                                switch($<multival.type>4) {
                                                    case (int)VALINT:
                                                        fprintf(outFile, "int");
                                                        break;
                                                    case (int)VALSTR:
                                                        fprintf(outFile, "string");
                                                        break;
                                                    case (int)VALBOOL:
                                                        fprintf(outFile, "boolean");
                                                        break;
                                                    default:
                                                        yyerror("Translation assignment failed");
                                                        break;
                                                }
                                                fprintf(outFile, " %s.%s\n", filename, chosenTable->strList[foundIndex]);
                                            } else {
                                                outTabs();
                                                fprintf(outFile, "istore %d\n", chosenTable->labels[foundIndex]);
                                            }
                                        } else {
                                            return yyerror("Error: Left hand side does not match right hand side value type");
                                        }

                                        // isWritingLocalVar = false;

                                        // Check whether this is array or not AND grammar exception
                                        // if($2 == -1) { // grammar WITHOUT array reference
                                        //     // if(chosenTable->storeType[foundIndex] == MULTIVAR) {
                                        //     //     return yyerror("Error: Expressions of array must have reference form");
                                        //     // } 

                                            
                                        // } 
                                        // else { // grammar WITH array reference
                                        //     if(chosenTable->storeType[foundIndex] != MULTIVAR) {
                                        //         return yyerror("Error: Non array expression must not have reference form");
                                        //     } 

                                        //     // check start index
                                        //     int sIndex = chosenTable->arrayAtr[foundIndex].startIndex;
                                        //     if($2 < sIndex) {
                                        //         return yyerror("Error: Wrong start index");
                                        //     }

                                        //     // TODO: Do assignment to array
                                        //     if(chosenTable->valueType[foundIndex] == VALINT && $<multival.type>4 == (int)VALREAL) {
                                        //         printf("Convert real(rhs) to int(lhs)! Do assignment\n");
                                        //     } else if((int)chosenTable->valueType[foundIndex] == $<multival.type>4) {
                                        //         printf("Value match! Do assignment\n");
                                        //     } else {
                                        //         return yyerror("Error: Left hand side does not match right hand side value type");
                                        //     }
                                        // }
                                    }
    | PUT { outTabs(); fprintf(outFile, "getstatic java.io.PrintStream java.lang.System.out\n"); isWritingLocalVar = true;} 
    exp {
                                        switch($<multival.type>3) {
                                            case (int)VALINT:
                                                // printf("arth_exp: %d\n", $<multival.value.uint>2);
                                                outTabs(); 
                                                fprintf(outFile, "invokevirtual void java.io.PrintStream.print(int)\n");
                                                break;
                                            case (int)VALSTR:
                                                // printf("string: %s\n", $<multival.value.ustr>2);
                                                outTabs(); 
                                                fprintf(outFile, "invokevirtual void java.io.PrintStream.print(java.lang.String)\n");
                                                break;  
                                            case (int)VALBOOL:
                                                // printf("bool_exp: %d\n", $<multival.value.ubool>2);
                                                outTabs(); 
                                                fprintf(outFile, "invokevirtual void java.io.PrintStream.print(boolean)\n");
                                                break;
                                            case (int)VALARRAYREF:
                                                printf("array: print whole array");
                                                break;
                                        }
                                        // isWritingLocalVar = false;
                                    }
    /* NO GET STATEMENT */
    // | GET ID array_ref              {
    //                                     struct SymbolTable* chosenTable = NULL;
    //                                     struct SymbolTable* parentTable = NULL;
    //                                     int foundIndex;

    //                                     // Check whether is initialized
    //                                     if(Col_ST[currentTable]->storeType[$2] == NONE) {
    //                                         // Search in its ancestor symbol table
    //                                         bool found = false;
    //                                         parentTable = Col_ST[currentTable]->parent;
    //                                         while (!found) {
    //                                             if(parentTable == NULL) {
    //                                                 return yyerror("Error: Identifier is not assigned yet anywhere");
    //                                             }
                                                
    //                                             foundIndex = lookupInTable(Col_ST[currentTable]->strList[$2], parentTable);
    //                                             if(foundIndex != -1 && parentTable->storeType[foundIndex] != NONE) {
    //                                                 found = true;
    //                                                 break;
    //                                             }

    //                                             parentTable = parentTable->parent;
    //                                         }
    //                                         printf("FOUND in parent %s (table %d, index %d)! for get ID\n", Col_ST[currentTable]->strList[$2], parentTable->index, foundIndex);
                                            
    //                                         // CHOSEN TABLE TO PARENT TABLE
    //                                         chosenTable = parentTable;
    //                                     }

    //                                     if(parentTable == NULL) {
    //                                         // JUST FOR CURRENT TABLE VARIABLE
    //                                         chosenTable = Col_ST[currentTable];
    //                                         foundIndex = $2;
    //                                     }
                                        
    //                                     if(chosenTable->storeType[foundIndex] == CONSTANT) {
    //                                         return yyerror("Error: Cannot process input to constant variables");
    //                                     }

    //                                     if(chosenTable->storeType[foundIndex] == FUNC) {
    //                                         return yyerror("Error: Cannot process input to functions");
    //                                     }

    //                                     if(chosenTable->storeType[foundIndex] == PROC) {
    //                                         return yyerror("Error: Cannot process input to procedures");
    //                                     }

    //                                     // Check whether this is array or not AND grammar exception
    //                                     if($2 == -1) { // grammar WITHOUT array reference
    //                                         if(chosenTable->storeType[foundIndex] == MULTIVAR) {
    //                                             return yyerror("Error: Expressions of array must have reference form");
    //                                         } 

    //                                         // TODO: Do scanning into normal variable
    //                                         printf("Scan input to noraml variable");                                            

    //                                     } else { // grammar WITH array reference
    //                                         if(chosenTable->storeType[foundIndex] != MULTIVAR) {
    //                                             return yyerror("Error: Non array expression must not have reference form");
    //                                         } 

    //                                         // check start index
    //                                         int sIndex = chosenTable->arrayAtr[foundIndex].startIndex;
    //                                         if($3 < sIndex) {
    //                                             return yyerror("Error: Wrong start index");
    //                                         }

    //                                         // TODO: Do scanning into array
    //                                         printf("Scan input to noraml variable"); 
    //                                     }
    //                                 }
    | RESULT exp                    {
                                        if(procScope) {
                                            return yyerror("Error: RESULT EXPRESSION is only used in function scope");
                                        }

                                        if(funcScope) {
                                            // check return type
                                            int expType = $<multival.type>2;
                                            if (expType == scopeReturnType) {
                                                // ok
                                                outTabs();
                                                fprintf(outFile, "ireturn\n");
                                            } else{
                                                return yyerror("Error: Expression of result must be the same as return type of the function");
                                            }
                                        }
                                    }        
    | RETURN                        {
                                        if(funcScope) {
                                            return yyerror("Error: RETURN is only used in procedure scope");
                                        }

                                        if(procScope) {
                                            // do nothing
                                            outTabs();
                                            fprintf(outFile, "return\n");
                                        }
                                    }
    | EXIT                          {isWritingLocalVar = true;}
    exit_cond                       {
                                        // isWritingLocalVar = false;
                                    }
    | SKIP                          {
                                        outTabs(); 
                                        fprintf(outFile, "getstatic java.io.PrintStream java.lang.System.out\n");
                                        outTabs(); 
                                        fprintf(outFile, "invokevirtual void java.io.PrintStream.println()\n");
                                        
                                    }
    ;
/* Exit Condition */
exit_cond: /* empty */      {
                                outTabs();
                                fprintf(outFile, "goto L%d\n", lStack[lStackPointer--]);
                            }
    | WHEN exp  {
                    if($<multival.type>2 != (int)VALBOOL) {
                        return yyerror("Error: Exit condition must be bool expression");
                    }

                    outTabs();
                    fprintf(outFile, "ifne L%d\n", lStack[lStackPointer--]);
                }

/* Expression Forms */
exp:
    /* Parantheses */
    POPEN exp PCLOSE           { 
                                    // if($<multival.type>2 == (int)VALSTR) {
                                    //     return yyerror("Error: Parentheses cannot accept string");
                                    // }

                                    if(!isWritingLocalVar) {
                                        if($<multival.type>2 == (int)VALINT) {
                                            $<multival.value.uint>$ = $<multival.value.uint>2; 
                                            $<multival.type>$ = (int)VALINT;
                                            // printf("%lf\n", $<multival.value.ureal>$);
                                        } else if($<multival.type>2 == (int)VALREAL) {
                                            $<multival.value.ureal>$ = $<multival.value.ureal>2; 
                                            $<multival.type>$ = (int)VALREAL;
                                            // printf("%lf\n", $<multival.value.ureal>$);
                                        } else if ($<multival.type>2 == (int)VALBOOL) {
                                            $<multival.value.ubool>$ = $<multival.value.ubool>2; 
                                            $<multival.type>$ = (int)VALBOOL;
                                            // printf("%d\n", $<multival.value.ubool>$);
                                        } else if ($<multival.type>2 == (int)VALSTR) {
                                            $<multival.value.ustr>$ = $<multival.value.ustr>2; 
                                            $<multival.type>$ = (int)VALSTR;
                                            // printf("%d\n", $<multival.value.ubool>$);
                                        }
                                    } else {
                                        $<multival.type>$ = $<multival.type>2;
                                    }
                                }   
    /* Arithmetic Expression */
    | arth_exp                  { 
                                    if($<multival.type>1 != (int)VALINT) {
                                        return yyerror("Error: Arithmethic expression only can receive INTEGER");
                                    }
                                    
                                    
                                    $<multival.type>$ = (int)VALINT;
                                    if(!isWritingLocalVar) {
                                        $<multival.value.uint>$ = $<multival.value.uint>1;
                                    }
                                    // printf("%lf\n", $<multival.value.ureal>$);

                                }
    /* Boolean Expression */
    | bool_exp                  {
        
                                    $<multival.type>$ = (int)VALBOOL;
                                    if(!isWritingLocalVar) {
                                        $<multival.value.ubool>$ = $1;
                                    }
                                }
    /* Expression Components */
    | term                      {
                                    if(!isWritingLocalVar) {
                                        switch($<multival.type>1) {
                                            case (int)VALINT:
                                                $<multival.value.uint>$ = $<multival.value.uint>1;
                                                $<multival.type>$ = (int)VALINT;
                                                break;
                                            case (int)VALREAL:
                                                $<multival.value.ureal>$ = $<multival.value.ureal>1;
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALSTR:
                                                $<multival.value.ustr>$ = strdup($<multival.value.ustr>1);
                                                $<multival.type>$ = (int)VALSTR;
                                                break;  
                                            case (int)VALBOOL:
                                                $<multival.value.ubool>$ = $<multival.value.ubool>1;
                                                $<multival.type>$ = (int)VALBOOL;
                                                break;
                                            case (int)VALARRAYREF:
                                                $<multival.value.uarray.arrayType>$ = $<multival.value.uarray.arrayType>1;
                                                $<multival.value.uarray.startIndex>$ = $<multival.value.uarray.startIndex>1;
                                                $<multival.value.uarray.capacity>$ = $<multival.value.uarray.capacity>1;
                                                $<multival.type>$ = (int)VALARRAYREF;
                                                break;
                                        }
                                    } else {
                                        $<multival.type>$ = $<multival.type>1;
                                    }
                                }
    ;

bool_exp:  
    exp RELOPLT exp           { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Bool expressions only can receive INTEGER comparison");
                                    }
                                        
                                    if(!isWritingLocalVar) {
                                        $$ = $<multival.value.uint>1 < $<multival.value.uint>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "isub\n");
                                        outTabs();
                                        fprintf(outFile, "iflt L%d\n", lCounter++);
                                        outTabs();
                                        fprintf(outFile, "iconst_0\n");
                                        outTabs();
                                        fprintf(outFile, "goto L%d\n", lCounter++);
                                        fprintf(outFile, "L%d:\n", lCounter-2);
                                        outTabs();
                                        fprintf(outFile, "iconst_1\n");
                                        fprintf(outFile, "L%d: \n", lCounter-1);
                                    }
                                }
    | exp RELOPLE exp           { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Bool expressions only can receive INTEGER comparison");
                                    }

                                    if(!isWritingLocalVar) {
                                        $$ = $<multival.value.uint>1 <= $<multival.value.uint>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "isub\n");
                                        outTabs();
                                        fprintf(outFile, "ifle L%d\n", lCounter++);
                                        outTabs();
                                        fprintf(outFile, "iconst_0\n");
                                        outTabs();
                                        fprintf(outFile, "goto L%d\n", lCounter++);
                                        fprintf(outFile, "L%d:\n", lCounter-2);
                                        outTabs();
                                        fprintf(outFile, "iconst_1\n");
                                        fprintf(outFile, "L%d: \n", lCounter-1);
                                    }
                                }
    | exp RELOPGT exp           { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Bool expressions only can receive INTEGER comparison");
                                    }

                                    if(!isWritingLocalVar) {
                                        $$ = $<multival.value.uint>1 > $<multival.value.uint>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "isub\n");
                                        outTabs();
                                        fprintf(outFile, "ifgt L%d\n", lCounter++);
                                        outTabs();
                                        fprintf(outFile, "iconst_0\n");
                                        outTabs();
                                        fprintf(outFile, "goto L%d\n", lCounter++);
                                        fprintf(outFile, "L%d:\n", lCounter-2);
                                        outTabs();
                                        fprintf(outFile, "iconst_1\n");
                                        fprintf(outFile, "L%d: \n", lCounter-1);
                                    }
                                }
    | exp RELOPGE exp           { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Bool expressions only can receive INTEGER comparison");
                                    }
                                    
                                    if(!isWritingLocalVar) {
                                        $$ = $<multival.value.uint>1 >= $<multival.value.uint>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "isub\n");
                                        outTabs();
                                        fprintf(outFile, "ifge L%d\n", lCounter++);
                                        outTabs();
                                        fprintf(outFile, "iconst_0\n");
                                        outTabs();
                                        fprintf(outFile, "goto L%d\n", lCounter++);
                                        fprintf(outFile, "L%d:\n", lCounter-2);
                                        outTabs();
                                        fprintf(outFile, "iconst_1\n");
                                        fprintf(outFile, "L%d: \n", lCounter-1);
                                    }
                                }
    | exp RELOPEQ exp           { 
                                    if($<multival.type>1 != $<multival.type>3) {
                                        return yyerror("Error: Bool expressions only can receive INTEGER comparison");
                                    }
                                    
                                    // --- RECHECK and COMPARE---

                                    if(!isWritingLocalVar) {
                                        if($<multival.type>1 == (int)VALINT || $<multival.type>3 == (int)VALINT) {
                                            // Compare REAL
                                            $$ = $<multival.value.uint>1 == $<multival.value.uint>3; 

                                        } else if($<multival.type>1 == (int)VALREAL || $<multival.type>3 == (int)VALREAL) {
                                            // Compare REAL
                                            $$ = $<multival.value.ureal>1 == $<multival.value.ureal>3; 

                                        } else if($<multival.type>1 == (int)VALSTR || $<multival.type>3 == (int)VALSTR) {
                                            // Compare STRING
                                            if(strcmp($<multival.value.ustr>1, $<multival.value.ustr>3) == 0) {
                                                $$ = true;
                                            } else {
                                                $$ = false;
                                            }

                                            // Need to free cuz of strdup previously
                                            free($<multival.value.ustr>1);
                                            free($<multival.value.ustr>3);

                                        } else if($<multival.type>1 == (int)VALBOOL || $<multival.type>3 == (int)VALBOOL) {
                                            // Compare BOOL
                                            $$ = $<multival.value.ubool>1 == $<multival.value.ubool>3; 
                                        }
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "isub\n");
                                        outTabs();
                                        fprintf(outFile, "ifeq L%d\n", lCounter++);
                                        outTabs();
                                        fprintf(outFile, "iconst_0\n");
                                        outTabs();
                                        fprintf(outFile, "goto L%d\n", lCounter++);
                                        fprintf(outFile, "L%d:\n", lCounter-2);
                                        outTabs();
                                        fprintf(outFile, "iconst_1\n");
                                        fprintf(outFile, "L%d: \n", lCounter-1);
                                    }
                                }
    | exp RELOPNE exp           { 
                                    if($<multival.type>1 != $<multival.type>3) {
                                        return yyerror("Error: Cannot compare different expression type");
                                    }
                                    
                                    // --- RECHECK and COMPARE---

                                    if(!isWritingLocalVar) {
                                        if($<multival.type>1 == (int)VALINT || $<multival.type>3 == (int)VALINT) {
                                            // Compare REAL
                                            $$ = $<multival.value.uint>1 == $<multival.value.uint>3; 

                                        } else if($<multival.type>1 == (int)VALREAL || $<multival.type>3 == (int)VALREAL) {
                                            // Compare REAL
                                            $$ = $<multival.value.ureal>1 != $<multival.value.ureal>3; 

                                        } else if($<multival.type>1 == (int)VALSTR || $<multival.type>3 == (int)VALSTR) {
                                            // Compare STRING
                                            if(strcmp($<multival.value.ustr>1, $<multival.value.ustr>3) != 0) {
                                                $$ = true;
                                            } else {
                                                $$ = false;
                                            }

                                            // Need to free cuz of strdup previously
                                            free($<multival.value.ustr>1);
                                            free($<multival.value.ustr>3);

                                        } else if($<multival.type>1 == (int)VALBOOL || $<multival.type>3 == (int)VALBOOL) {
                                            // Compare BOOL
                                            $$ = $<multival.value.ubool>1 != $<multival.value.ubool>3; 
                                        }
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "isub\n");
                                        outTabs();
                                        fprintf(outFile, "ifne L%d\n", lCounter++);
                                        outTabs();
                                        fprintf(outFile, "iconst_0\n");
                                        outTabs();
                                        fprintf(outFile, "goto L%d\n", lCounter++);
                                        fprintf(outFile, "L%d:\n", lCounter-2);
                                        outTabs();
                                        fprintf(outFile, "iconst_1\n");
                                        fprintf(outFile, "L%d: \n", lCounter-1);
                                    }
                                }
    | exp AND exp               { 
                                    if($<multival.type>1 != (int)VALBOOL || $<multival.type>3 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }

                                    if(!isWritingLocalVar) {
                                        $$ = $<multival.value.ubool>1 && $<multival.value.ubool>3; 
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "iand\n");
                                    }
                                }
    | exp OR exp                { 
                                    if($<multival.type>1 != (int)VALBOOL || $<multival.type>3 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }
                                    
                                    if(!isWritingLocalVar) {
                                        $$ = $<multival.value.ubool>1 || $<multival.value.ubool>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "ior\n");
                                    }
                                }
    | NOT {outTabs(); fprintf(outFile, "iconst_1\n");} exp                   { 
                                    if($<multival.type>3 != (int)VALBOOL) {
                                        return yyerror("Error: Boolean expressions can only receive boolean");
                                    }
                                    
                                    if(!isWritingLocalVar) {
                                        $$ = !$<multival.value.ubool>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "ixor\n");
                                    }
                                }
    ;

// change to int with int
// also change bool exp
arth_exp:  
    exp ADD exp                 { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Expressions only can receive INTEGER");
                                    }

                                    
                                    $<multival.type>$ = (int)VALINT;
                                    if(!isWritingLocalVar) {
                                        $<multival.value.uint>$ = $<multival.value.uint>1 + $<multival.value.uint>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "iadd\n");
                                    }
                                }
    | exp SUB exp               { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Expressions only can receive INTEGER");
                                    }

                                    
                                    $<multival.type>$ = (int)VALINT;
                                    if(!isWritingLocalVar) {
                                        $<multival.value.uint>$ = $<multival.value.uint>1 - $<multival.value.uint>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "isub\n");
                                    }
                                }
    | exp MUL exp               { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Expressions only can receive INTEGER");
                                    }

                                    $<multival.type>$ = (int)VALINT;
                                    if(!isWritingLocalVar) {
                                        $<multival.value.uint>$ = $<multival.value.uint>1 * $<multival.value.uint>3;
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "imul\n");
                                    }
                                }
    | exp DIV exp               { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Expressions only can receive INTEGER");
                                    }

                                    $<multival.type>$ = (int)VALINT;
                                    if($<multival.value.uint>3 == 0) return yyerror("divide by zero");
                                    else {
                                        if(!isWritingLocalVar) {
                                            $<multival.value.uint>$ = $<multival.value.uint>1 / $<multival.value.uint>3;
                                        } else {
                                            outTabs();
                                            fprintf(outFile, "idiv\n");
                                        }
                                    }      
                                }
    | exp MOD exp               { 
                                    if($<multival.type>1 != (int)VALINT || $<multival.type>3 != (int)VALINT) {
                                        return yyerror("Error: Expressions only can receive INTEGER");
                                    }

                                    
                                    $<multival.type>$ = (int)VALINT;
                                    if(!isWritingLocalVar) {
                                        $<multival.value.uint>$ = fmod($<multival.value.uint>1, $<multival.value.uint>3);
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "irem\n");
                                    }
                                }
    | SUB exp %prec UMINUS      { 
                                    if($<multival.type>2 != (int)VALINT) {
                                        return yyerror("Error: Expressions only can receive INTEGER");
                                    }
                                    
                                    
                                    $<multival.type>$ = (int)VALINT;
                                    if(!isWritingLocalVar) {
                                        $<multival.value.uint>$ = -$<multival.value.uint>2; 
                                    } else {
                                        outTabs();
                                        fprintf(outFile, "ineg\n");
                                    }
                                }
    ;


term:
    /* variable names and array reference */
    ID          { 
                    topTerm++;
                    termStack[topTerm].chosenTableStack = NULL;
                    termStack[topTerm].foundIndex = -1;
                    struct SymbolTable* parentTable = NULL;
                    // Check whether is initialized
                    if(Col_ST[currentTable]->storeType[$1] == NONE) {
                        // Search in its ancestor symbol table
                        bool found = false;
                        
                        parentTable = Col_ST[currentTable]->parent;
                        while (!found) {
                            if(parentTable == NULL) {
                                return yyerror("Error: Identifier is not assigned yet anywhere");
                            }
                            
                            termStack[topTerm].foundIndex = lookupInTable(Col_ST[currentTable]->strList[$1], parentTable);
                            if(termStack[topTerm].foundIndex != -1 && parentTable->storeType[termStack[topTerm].foundIndex] != NONE) {
                                found = true;
                                break;
                            }

                            parentTable = parentTable->parent;
                        }
                        printf("FOUND in parent %s (table %d, index %d)! for const oprnd\n", Col_ST[currentTable]->strList[$1], parentTable->index, termStack[topTerm].foundIndex );
                        
                        // CHOSEN TABLE TO PARENT TABLE
                        termStack[topTerm].chosenTableStack = parentTable;
                    }

                    if(parentTable == NULL) {
                        // JUST FOR CURRENT TABLE VARIABLE
                        termStack[topTerm].chosenTableStack = Col_ST[currentTable];
                        termStack[topTerm].foundIndex = $1;
                    }

                    if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == PROC) {
                        return yyerror("Error: Cannot get procedures");
                    }

                    if(needConstExp) {
                        if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] != CONSTANT) {
                            return yyerror("Error: Declaration must not have non constant expression");
                        }
                    }

                    if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == FUNC) {
                        // funcCalling = true;
                        // atParentIndex = termStack[topTerm].foundIndex;
                        // currentParamIndex = 0;

                        topFuncCalling++;
                        funcCallingStack[topFuncCalling].funcCalling = true; 
                        funcCallingStack[topFuncCalling].atParentIndex = termStack[topTerm].foundIndex;
                        funcCallingStack[topFuncCalling].currentParamIndex = 0;
                        functionParent =  termStack[topTerm].chosenTableStack;
                        // functionParent = Col_ST[0];
                    }
                } 
    id_ext                      { 
                                    // Check whether this is array or not AND grammar exception
                                    if($3 == NO_EXT && termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == CONSTANT) { 
                                        // normal const

                                        // TODO: Return constant value
                                        if(!isWritingLocalVar) {
                                            switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                                case (int)VALINT:
                                                    $<multival.value.uint>$ = termStack[topTerm].chosenTableStack->value[termStack[topTerm].foundIndex].integer; // get integer
                                                    $<multival.type>$ = (int)VALINT;
                                                    break;
                                                case (int)VALSTR:
                                                    $<multival.value.ustr>$ = termStack[topTerm].chosenTableStack->value[termStack[topTerm].foundIndex].string; // get integer
                                                    $<multival.type>$ = (int)VALSTR;
                                                    break;
                                                case (int)VALBOOL:
                                                    $<multival.value.ubool>$ = termStack[topTerm].chosenTableStack->value[termStack[topTerm].foundIndex].boolean; // get bool
                                                    $<multival.type>$ = (int)VALBOOL;
                                                    break;
                                            }
                                        } else {
                                            struct MultiValue data;
                                            switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                                case (int)VALINT:
                                                    data.type = (int) VALINT;
                                                    data.value.integer = termStack[topTerm].chosenTableStack->value[termStack[topTerm].foundIndex].integer;
                                                    $<multival.type>$ = (int)VALINT;
                                                    break;
                                                case (int)VALSTR:
                                                    data.type = (int) VALSTR;
                                                    data.value.string = termStack[topTerm].chosenTableStack->value[termStack[topTerm].foundIndex].string;
                                                    $<multival.type>$ = (int)VALSTR;
                                                    break;
                                                case (int)VALBOOL:
                                                    data.type = (int) VALBOOL;
                                                    data.value.boolean = termStack[topTerm].chosenTableStack->value[termStack[topTerm].foundIndex].boolean;
                                                    $<multival.type>$ = (int)VALBOOL;
                                                    break;
                                            }
                                            transExprTerm(data);
                                        }
                                        

                                    } else if($3 == NO_EXT && termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == VARIABLE) { 
                                        // normal variable

                                        if(!isWritingLocalVar) {
                                            // ??
                                        } else {
                                            switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                                case (int)VALINT:
                                                    outTabs();
                                                    if(termStack[topTerm].chosenTableStack->index == 0)
                                                        fprintf(outFile, "getstatic int %s.%s\n", filename, termStack[topTerm].chosenTableStack->strList[termStack[topTerm].foundIndex]);
                                                    else 
                                                        fprintf(outFile, "iload %d\n", termStack[topTerm].chosenTableStack->labels[termStack[topTerm].foundIndex]);
                                                    $<multival.type>$ = (int)VALINT;
                                                    break;
                                                case (int)VALBOOL:
                                                    outTabs();
                                                    if(termStack[topTerm].chosenTableStack->index == 0)
                                                        fprintf(outFile, "getstatic bool %s.%s\n", filename, termStack[topTerm].chosenTableStack->strList[termStack[topTerm].foundIndex]);
                                                    else 
                                                        fprintf(outFile, "iload %d\n", termStack[topTerm].chosenTableStack->labels[termStack[topTerm].foundIndex]);
                                                    $<multival.type>$ = (int)VALBOOL;
                                                    break;
                                            }

                                        }
                                        

                                    } else if($3 == NO_EXT && termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == FUNC) {
                                        // Function with no param
                                        return yyerror("Error: Function expression must have parentheses");

                                    } else if($3 == FUNCPARAM && termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == FUNC) {
                                        // Function with param
                                        // TODO: Return normal function with value 
                                        funcCallingStack[topFuncCalling].funcCalling = false; 
                                        funcCallingStack[topFuncCalling].atParentIndex = -1;
                                        topFuncCalling--;
                                        // switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                        //     case (int)VALINT:
                                        //         $<multival.value.uint>$ = 997; // get integer
                                        //         $<multival.type>$ = (int)VALINT;
                                        //         break;
                                        //     case (int)VALREAL:
                                        //         $<multival.value.ureal>$ = 997; // get real
                                        //         $<multival.type>$ = (int)VALREAL;
                                        //         break;
                                        //     case (int)VALSTR:
                                        //         $<multival.value.ustr>$ = "Temp String funcwithparam"; // get string
                                        //         $<multival.type>$ = (int)VALSTR;
                                        //         break;  
                                        //     case (int)VALBOOL:
                                        //         $<multival.value.ubool>$ = true; // get bool
                                        //         $<multival.type>$ = (int)VALBOOL;
                                        //         break;
                                        // }
                                        $<multival.type>$ = (int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex];
                                        outTabs();
                                        fprintf(outFile, "invokestatic ");
                                        switch(termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                            case VALINT:
                                                fprintf(outFile, "int ");
                                                break;
                                            case VALBOOL:
                                                fprintf(outFile, "boolean ");
                                                break;
                                        }
                                        fprintf(outFile, "%s.%s(", filename, termStack[topTerm].chosenTableStack->strList[termStack[topTerm].foundIndex]);
                                        for(int i = 0; i < termStack[topTerm].chosenTableStack->funcAtr[termStack[topTerm].foundIndex].size; i++) {
                                            if(i != 0) {
                                                fprintf(outFile, ", ");
                                            }
                                            switch(termStack[topTerm].chosenTableStack->funcAtr[termStack[topTerm].foundIndex].paramType[i]) {
                                                case VALINT:
                                                    fprintf(outFile, "int");
                                                    break;
                                                case VALBOOL:
                                                    fprintf(outFile, "boolean");
                                                    break;
                                            }
                                        }
                                        fprintf(outFile, ")\n");

                                        resetFunctionGrammarFlags();
                                    } else if($3 == FUNCPARAM && (termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == CONSTANT || termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] == VARIABLE)) {
                                        return yyerror("Error: Variable expression must not have parentheses");
                                    } 
                                    // else { // grammar WITH array reference
                                    //     if(termStack[topTerm].chosenTableStack->storeType[termStack[topTerm].foundIndex] != MULTIVAR) {
                                    //         return yyerror("Error: Non array expression must not have reference form");
                                    //     } 

                                    //     // check start index
                                    //     int sIndex = termStack[topTerm].chosenTableStack->arrayAtr[termStack[topTerm].foundIndex].startIndex;
                                    //     if($3 < sIndex) {
                                    //         return yyerror("Error: Wrong start index");
                                    //     }

                                    //     // TODO: Return array value
                                    //     switch((int)termStack[topTerm].chosenTableStack->valueType[termStack[topTerm].foundIndex]) {
                                    //         case (int)VALINT:
                                    //             $<multival.value.uint>$ = 991; // get integer
                                    //             $<multival.type>$ = (int)VALINT;
                                    //             break;
                                    //         case (int)VALREAL:
                                    //             $<multival.value.ureal>$ = 991; // get real
                                    //             $<multival.type>$ = (int)VALREAL;
                                    //             break;
                                    //         case (int)VALSTR:
                                    //             $<multival.value.ustr>$ = "Temp String for array"; // get string
                                    //             $<multival.type>$ = (int)VALSTR;
                                    //             break;  
                                    //         case (int)VALBOOL:
                                    //             $<multival.value.ubool>$ = false; // get bool
                                    //             $<multival.type>$ = (int)VALBOOL;
                                    //             break;
                                    //     }
                                    // }

                                    //reset
                                    termStack[topTerm].chosenTableStack = NULL;
                                    termStack[topTerm].foundIndex = -1;
                                    topTerm--;
                                }
    /* Literal Constant */
    | const_oprnd               {
                                    if(!isWritingLocalVar) {
                                        switch($<multival.type>1) {
                                            case (int)VALINT:
                                                $<multival.value.uint>$ = $<multival.value.uint>1;
                                                $<multival.type>$ = (int)VALINT;
                                                break;
                                            case (int)VALREAL:
                                                $<multival.value.ureal>$ = $<multival.value.ureal>1;
                                                $<multival.type>$ = (int)VALREAL;
                                                break;
                                            case (int)VALSTR:
                                                $<multival.value.ustr>$ = strdup($<multival.value.ustr>1);
                                                $<multival.type>$ = (int)VALSTR;
                                                break;  
                                            case (int)VALBOOL:
                                                $<multival.value.ubool>$ = $<multival.value.ubool>1;
                                                $<multival.type>$ = (int)VALBOOL;
                                                break;
                                        }
                                    } else {
                                        struct MultiValue data;
                                        switch($<multival.type>1) {
                                            case (int)VALINT:
                                                data.type = (int) VALINT;
                                                data.value.integer = $<multival.value.uint>1;
                                                break;
                                            case (int)VALSTR:
                                                data.type = (int) VALSTR;
                                                data.value.string = $<multival.value.ustr>1;
                                                break;  
                                            case (int)VALBOOL:
                                                data.type = (int) VALBOOL;
                                                data.value.boolean = $<multival.value.ubool>1;
                                                break;
                                        }
                                        
                                        $<multival.type>$ = $<multival.type>1;
                                        transExprTerm(data);
                                    }
                                }
    ;

const_oprnd: STRING             { $<multival.value.ustr>$ = $1; $<multival.type>$ = (int)VALSTR; }
    | INT                       { $<multival.value.uint>$ = $1; $<multival.type>$ = (int)VALINT; }
    | REAL                      { $<multival.value.ureal>$ = $1; $<multival.type>$ = (int)VALREAL; }
    | TRUE                      { $<multival.value.ubool>$ = true; $<multival.type>$ = (int)VALBOOL; }
    | FALSE                     { $<multival.value.ubool>$ = false; $<multival.type>$ = (int)VALBOOL; }
    ;

/* Check whether it has [exp] or not after ID*/
// array_ref: /* empty */  { $$ = -1; }
//     | SOPEN exp SCLOSE  {   
//                             if($<multival.value.ureal>2 < 0) {
//                                 return yyerror("Error: Invalid index range");
//                             } 
                            
//                             $$ = (int)$<multival.value.ureal>2; 
//                         }
//     ;

/* Check whether it has [exp] or (parameter) or not after ID*/
id_ext: /* empty */         { $$ = NO_EXT; }
    | SOPEN exp SCLOSE      {   
                                if($<multival.value.ureal>2 < 0) {
                                    return yyerror("Error: Invalid index range");
                                } 
                                
                                $$ = (int)$<multival.value.ureal>2; 
                            } 
    | POPEN param PCLOSE    { 
                                $$ = FUNCPARAM;
                                int calledParameter = functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].size;
                                printf("%d><%d\n", funcCallingStack[topFuncCalling].currentParamIndex, calledParameter);

                                if(funcCallingStack[topFuncCalling].currentParamIndex != calledParameter) {
                                    return yyerror("Number of parameter does not match");
                                } else {
                                    printf("Parameter counting PASS %d\n ", calledParameter);
                                }
                            };

/* Procedure Invocation */
invoc_param: POPEN param PCLOSE 
    ;
param: /*empty*/ 
    | exp           {
                        if(funcCallingStack[topFuncCalling].funcCalling) {
                            printf("top funcCalling: %d\n", topFuncCalling);
                            // if(isCallingArray
                            //     && $<multival.value.uarray.arrayType>1 == functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].paramType[funcCallingStack[topFuncCalling].currentParamIndex] 
                            //     && $<multival.value.uarray.capacity>1 == functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].arrayAtr[funcCallingStack[topFuncCalling].currentParamIndex].capacity) {
                                
                            //     printf("same\n");
                            // } else 
                            if(
                                $<multival.type>1 == functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].paramType[funcCallingStack[topFuncCalling].currentParamIndex]
                                // || ($<multival.type>1 == (int)VALREAL && functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].paramType[funcCallingStack[topFuncCalling].currentParamIndex] == (int)VALINT)
                            ) {
                                printf("same\n");
                            } else {
                                return yyerror("Error: Parameter does not match");
                            }
                            funcCallingStack[topFuncCalling].currentParamIndex++;
                            isCallingArray = false;
                        }
                    } 
    next_exps
    ;
next_exps: /* empty */
    | COMMA param
    ;
proc_invoke: ID { 
                    // funcCalling = true; 
                    // atParentIndex = $1;

                    int foundIndex = -1;
                    if(Col_ST[currentTable]->storeType[$1] == NONE) {
                        // find index in table 0
                        foundIndex = lookupInTable(Col_ST[currentTable]->strList[$1], Col_ST[0]);
                        if(foundIndex == -1) {
                            return yyerror("Error: Procedure is not assigned yet");    
                        } 
                    }
                    
                    if(foundIndex == -1) foundIndex = $1;
                    if(Col_ST[0]->storeType[foundIndex] != PROC) {
                        return yyerror("Error: This identifier is not a procedure");
                    }

                    currentParamIndex = 0;
                    topFuncCalling++;
                    funcCallingStack[topFuncCalling].funcCalling = true; 
                    funcCallingStack[topFuncCalling].atParentIndex = foundIndex;
                    funcCallingStack[topFuncCalling].currentParamIndex = 0;
                    functionParent = Col_ST[0];
                    isWritingLocalVar = true;
                } 
    invoc_param {
                    int calledParameter = functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].size;
                    if(funcCallingStack[topFuncCalling].currentParamIndex != calledParameter) {
                        return yyerror("Number of parameter does not match");
                    } else {
                        printf("Parameter counting PASS %d\n", calledParameter);
                    }
                    outTabs();
                    fprintf(outFile, "invokestatic void ");
                    fprintf(outFile, "%s.%s(", filename, functionParent->strList[funcCallingStack[topFuncCalling].atParentIndex]);
                    for(int i = 0; i < functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].size; i++) {
                        if(i != 0) {
                            fprintf(outFile, ", ");
                        }
                        switch(functionParent->funcAtr[funcCallingStack[topFuncCalling].atParentIndex].paramType[i]) {
                            case VALINT:
                                fprintf(outFile, "int");
                                break;
                            case VALBOOL:
                                fprintf(outFile, "boolean");
                                break;
                        }
                    }
                    fprintf(outFile, ")\n");

                    funcCallingStack[topFuncCalling].funcCalling = false; 
                    funcCallingStack[topFuncCalling].atParentIndex = -1;
                    funcCallingStack[topFuncCalling].currentParamIndex = -1;
                    topFuncCalling--;
                    resetFunctionGrammarFlags();
                    // isWritingLocalVar = false;
                    // functionParent = NULL;
                }
    ;
/* ----------------------- */

/* IF statement */
if_stmt: IF                             {
                                            lCounter += 2;
                                            lStack[++lStackPointer] = lCounter - 1;
                                            lStack[++lStackPointer] = lCounter - 2;
                                            lStack[++lStackPointer] = lCounter - 1;
                                            lStack[++lStackPointer] = lCounter - 2;
                                            isWritingLocalVar = true;
                                        }
    exp                                 {
                                            if($<multival.type>3 != (int)VALBOOL) {
                                                return yyerror("Error: Conditional expression can only receive bool");
                                            }
                                            printf("Conditional Expression pass\n");
                                        } 
    THEN                                {
                                            outTabs();
                                            fprintf(outFile, "ifeq L%d\n", lStack[lStackPointer--]); // goto false
                                            createChildTable();
                                            printSymbols();
                                        } 
    // true statement
    stmt_body                           {
                                            outTabs();
                                            fprintf(outFile, "goto L%d\n", lStack[lStackPointer--]); // goto Lexit
                                        }
    else_exist                          
    END IF                              {
                                            fprintf(outFile, "L%d:\n", lStack[lStackPointer--]); // Lexit
                                            returnToParent();
                                            printSymbols();
                                        }
    ;

else_exist: /* empty */                 {
                                            fprintf(outFile, "L%d:\n", lStack[lStackPointer--]); // false statement
                                            outTabs();
                                            fprintf(outFile, "goto L%d\n", lStack[lStackPointer]);
                                        }
    | ELSE                              {
                                            returnToParent();
                                            printSymbols();

                                            fprintf(outFile, "L%d:\n", lStack[lStackPointer--]); // false statement


                                            createChildTable();
                                            printSymbols();
                                        } 
    stmt_body                           {
                                            outTabs();
                                            fprintf(outFile, "goto L%d\n", lStack[lStackPointer]);
                                        }
    ;
/* ----------------------- */

/* LOOP Statement */
loop_stmt: LOOP                         {
                                            createChildTable();
                                            printSymbols();

                                            lCounter += 2;
                                            lStack[++lStackPointer] = lCounter - 1;
                                            lStack[++lStackPointer] = lCounter - 2;
                                            lStack[++lStackPointer] = lCounter - 1;
                                            lStack[++lStackPointer] = lCounter - 2;

                                            fprintf(outFile, "L%d:\n", lStack[lStackPointer--]);
                                        } 
    stmt_body END LOOP                  {
                                            returnToParent();
                                            printSymbols();
                                            outTabs();
                                            fprintf(outFile, "goto L%d\n", lStack[lStackPointer--]);
                                            fprintf(outFile, "L%d:\n", lStack[lStackPointer--]);
                                        }
    | FOR                                       {
                                                    needConstExp = true;
                                                    lCounter += 2;
                                                    lStack[++lStackPointer] = lCounter - 1;
                                                    lStack[++lStackPointer] = lCounter - 2;
                                                    lStack[++lStackPointer] = lCounter - 1;
                                                    lStack[++lStackPointer] = lCounter - 2;
                                                } 
    for_ext ID                                  {
                                                    struct SymbolTable* chosenTable = NULL;
                                                    struct SymbolTable* parentTable = NULL;
                                                    int foundIndex = -1;

                                                    // Check whether is initialized
                                                    if(Col_ST[currentTable]->storeType[$4] == NONE) {
                                                        // Search in its ancestor symbol table
                                                        bool found = false;
                                                        parentTable = Col_ST[currentTable]->parent;
                                                        while (!found) {
                                                            if(parentTable == NULL) {
                                                                return yyerror("Error: Identifier is not assigned yet anywhere");
                                                            }
                                                            
                                                            foundIndex = lookupInTable(Col_ST[currentTable]->strList[$4], parentTable);
                                                            if(foundIndex != -1 && parentTable->storeType[foundIndex] != NONE) {
                                                                found = true;
                                                                break;
                                                            }
                                                            printf("FOUND in parent %s (table %d, index %d)! for get ID\n", Col_ST[currentTable]->strList[$4], parentTable->index, foundIndex);

                                                            parentTable = parentTable->parent;
                                                        }
                                                        printf("FOUND in parent! for FOR loop\n");
                                                        
                                                        // CHOSEN TABLE TO PARENT TABLE
                                                        chosenTable = parentTable;
                                                    }

                                                    if(parentTable == NULL) {
                                                        // JUST FOR CURRENT TABLE VARIABLE
                                                        chosenTable = Col_ST[currentTable];
                                                        foundIndex = $4;
                                                    }
                                                    
                                                    //CheckType
                                                    if(chosenTable->storeType[foundIndex] != VARIABLE || chosenTable->valueType[foundIndex] != VALINT) {
                                                        return yyerror("Error: Identifier is not VARIABLE type and not INTEGER type");
                                                    }

                                                    forVarPointer++;
                                                    if(chosenTable->index == 0) {   
                                                        forVarLabel[forVarPointer] = -1;
                                                        forVarIndex[forVarPointer] = foundIndex;
                                                    } else {
                                                        forVarLabel[forVarPointer] = chosenTable->labels[foundIndex];
                                                    }
                                                }
    COLON                                       { // need const expression
                                                    createChildTable(); 
                                                    printSymbols();
                                                    // needConstExp = false; // still need constExp
                                                    isWritingLocalVar = false;
                                                } 
    exp DOT DOT exp                             {
                                                    if(!isForDecr[forDecrPointer]) {
                                                        // increasing
                                                        printf("%d %d \n", $<multival.value.uint>8, $<multival.value.uint>11);
                                                        if($<multival.value.uint>8 > $<multival.value.uint>11) {
                                                            return yyerror("Error: Invalid for loop range, should be increasing");
                                                        }
                                                    } else {
                                                        // decreasing
                                                        if($<multival.value.uint>8 < $<multival.value.uint>11) {
                                                            return yyerror("Error: Invalid for loop range, should be decreasing");
                                                        }
                                                    }
                                                    // assignation to for's counter variable
                                                    outTabs();
                                                    fprintf(outFile, "sipush %d\n", $<multival.value.uint>8);
                                                    outTabs();
                                                    if(forVarLabel[forVarPointer] == -1) {
                                                        // global var
                                                        fprintf(outFile, "putstatic int ");
                                                        fprintf(outFile, " %s.%s\n", filename, Col_ST[0]->strList[forVarIndex[forVarPointer]]);
                                                    } else {
                                                        // local var
                                                        fprintf(outFile, "istore %d\n", forVarLabel[forVarPointer]); 
                                                    }

                                                    // start for
                                                    fprintf(outFile, "L%d:\n", lStack[lStackPointer--]); // Lbegin

                                                    // checking whether counter is in range
                                                    outTabs();
                                                    if(forVarLabel[forVarPointer] == -1) {
                                                        // global var
                                                        fprintf(outFile, "getstatic int ");
                                                        fprintf(outFile, " %s.%s\n", filename, Col_ST[0]->strList[forVarIndex[forVarPointer]]);
                                                    } else {
                                                        // local var
                                                        fprintf(outFile, "iload %d\n", forVarLabel[forVarPointer]);
                                                    }
                                                    outTabs();
                                                    fprintf(outFile, "sipush %d\n", $<multival.value.uint>11);
                                                    outTabs();
                                                    fprintf(outFile, "isub\n");
                                                    if(!isForDecr[forDecrPointer]) { // goto Ltrue
                                                        outTabs();
                                                        fprintf(outFile, "ifgt L%d\n", lCounter++); // increasing, exit when > 0
                                                    } else {
                                                        outTabs();
                                                        fprintf(outFile, "iflt L%d\n", lCounter++); // decreasing, exit when < 0
                                                    }
                                                    outTabs();
                                                    fprintf(outFile, "iconst_0\n");
                                                    outTabs();
                                                    fprintf(outFile, "goto L%d\n", lCounter++); // goto Lfalse
                                                    fprintf(outFile, "L%d:\n", lCounter-2); // Ltrue
                                                    outTabs();
                                                    fprintf(outFile, "iconst_1\n");
                                                    fprintf(outFile, "L%d: \n", lCounter-1); // Lfalse
                                                    outTabs();
                                                    fprintf(outFile, "ifne L%d\n", lStack[lStackPointer--]); // Lexit
                                                    needConstExp = false;
                                                    isWritingLocalVar = true;
                                                } 
    stmt_body                                   {
                                                    outTabs();
                                                    if(forVarLabel[forVarPointer] == -1) {
                                                        // global var
                                                        fprintf(outFile, "getstatic int ");
                                                        fprintf(outFile, " %s.%s\n", filename, Col_ST[0]->strList[forVarIndex[forVarPointer]]);
                                                    } else {
                                                        // local var
                                                        fprintf(outFile, "iload %d\n", forVarLabel[forVarPointer]);
                                                    }
                                                    outTabs();
                                                    fprintf(outFile, "sipush %d\n", 1);
                                                    outTabs();
                                                    if(!isForDecr[forDecrPointer]) {
                                                        fprintf(outFile, "iadd\n");
                                                    } else {
                                                        fprintf(outFile, "isub\n");
                                                    }
                                                    if(forVarLabel[forVarPointer] == -1) {
                                                        // global var
                                                        fprintf(outFile, "putstatic int ");
                                                        fprintf(outFile, " %s.%s\n", filename, Col_ST[0]->strList[forVarIndex[forVarPointer]]);
                                                    } else {
                                                        // local var
                                                        fprintf(outFile, "istore %d\n", forVarLabel[forVarPointer]); 
                                                    }
                                                    outTabs();
                                                    fprintf(outFile, "goto L%d\n", lStack[lStackPointer--]); // goto Lbegin
                                                } 
    END FOR                                     {
                                                    forVarPointer--;
                                                    fprintf(outFile, "L%d:\n", lStack[lStackPointer--]); // Lexit
                                                    returnToParent();
                                                    printSymbols();
                                                    forDecrPointer--;
                                                }
    ;
for_ext: /* empty */ {isForDecr[++forDecrPointer] = false;}
    | DECREASING    {isForDecr[++forDecrPointer] = true;}
/* ----------------------- */

%%


void create() {
    // Initiate Collection of Symbol Tables
    currentSize = 0;
    currentTable = -1;
    Col_ST = malloc(NUMBER_OF_ST * sizeof(struct SymbolTable*));

    createChildTable();
}

void createChildTable() {
    int oldTableIndex = currentTable;
    currentSize++;
    currentTable = currentSize - 1;
    Col_ST[currentTable] = malloc(sizeof(struct SymbolTable));
    Col_ST[currentTable]->index = currentTable;
    Col_ST[currentTable]->capacity = ST_LENGTH;
    Col_ST[currentTable]->size = 0;
    Col_ST[currentTable]->strList = malloc(Col_ST[currentTable]->capacity * sizeof(char*));
	Col_ST[currentTable]->valueType = malloc(Col_ST[currentTable]->capacity * sizeof(enum ValueType));
	Col_ST[currentTable]->storeType = malloc(Col_ST[currentTable]->capacity * sizeof(enum StoreType));
    Col_ST[currentTable]->value = malloc(Col_ST[currentTable]->capacity * sizeof(union Value));
    Col_ST[currentTable]->labels = malloc(Col_ST[currentTable]->capacity * sizeof(int));
    Col_ST[currentTable]->arrayAtr = malloc(Col_ST[currentTable]->capacity * sizeof(struct ArrayAtr));
    Col_ST[currentTable]->funcAtr = malloc(Col_ST[currentTable]->capacity * sizeof(struct FuncAtr));

	if(Col_ST[currentTable]->strList == NULL) {
        printf("Unable to allocate memory for strList:(\n");
        return;
    }

    if(Col_ST[currentTable]->valueType == NULL) {
        printf("Unable to allocate memory for valueType:(\n");
        return;
    }
    
    if(Col_ST[currentTable]->storeType == NULL) {
        printf("Unable to allocate memory for storeType:(\n");
        return;
    }

    if(Col_ST[currentTable]->arrayAtr == NULL) {
        printf("Unable to allocate memory for arrayAtr:(\n");
        return;
    }

    if(Col_ST[currentTable]->funcAtr == NULL) {
        printf("Unable to allocate memory for funcAtr:(\n");
        return;
    }

    if(currentTable > 0) {
        Col_ST[currentTable]->parent = Col_ST[oldTableIndex];
    } else {
        Col_ST[currentTable]->parent = NULL;
    }

    // printf("New table made. Checking..\n");
    // printf("Index: %d\n", Col_ST[currentTable]->index);
    // printf("Capacity: %lu\n", Col_ST[currentTable]->capacity);
    // printf("Size: %lu\n", Col_ST[currentTable]->size);

}

void returnToParent() {
    struct SymbolTable* parentTable = Col_ST[currentTable]->parent;
    currentTable = parentTable->index;
}

int lookup(char* s) {
    for(int i = 0; i < Col_ST[currentTable]->size; i++) {
		if(strcmp(Col_ST[currentTable]->strList[i], s) == 0) {
			return i;
		}
	}
	
	return -1;
}

int lookupInTable(char* s, struct SymbolTable* parentTable) {

    for(int i = 0; i < parentTable->size; i++) {
		if(strcmp(parentTable->strList[i], s) == 0) {
			return i;
		}
	}
	
	return -1;
}

int insert(char* s) {     
    if(Col_ST[currentTable]->size == Col_ST[currentTable]->capacity) {
        Col_ST[currentTable]->capacity *= 2;
        Col_ST[currentTable]->strList = realloc(Col_ST[currentTable]->strList, Col_ST[currentTable]->capacity * sizeof(char*));
		Col_ST[currentTable]->valueType = realloc(Col_ST[currentTable]->valueType, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->valueType));
		Col_ST[currentTable]->storeType = realloc(Col_ST[currentTable]->storeType, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->storeType));
        Col_ST[currentTable]->value = realloc(Col_ST[currentTable]->value, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->value));
        Col_ST[currentTable]->labels = realloc(Col_ST[currentTable]->labels, Col_ST[currentTable]->capacity * sizeof(int));
        Col_ST[currentTable]->arrayAtr = realloc(Col_ST[currentTable]->arrayAtr, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->arrayAtr));
        Col_ST[currentTable]->funcAtr = realloc(Col_ST[currentTable]->funcAtr, Col_ST[currentTable]->capacity * sizeof(*Col_ST[currentTable]->funcAtr));

        if(Col_ST[currentTable]->strList == NULL) {
            printf("Unable to reallocate memory :(\n");
            return -1;
        }
    }
	Col_ST[currentTable]->strList[Col_ST[currentTable]->size] = malloc(strlen(s) + 1);
	strcpy(Col_ST[currentTable]->strList[Col_ST[currentTable]->size], s);
    Col_ST[currentTable]->storeType[Col_ST[currentTable]->size] = NONE;
    Col_ST[currentTable]->valueType[Col_ST[currentTable]->size] = VALNONE;

	Col_ST[currentTable]->size++;
	
	return Col_ST[currentTable]->size - 1;
}

void dump() {
	printf("\n");
	printSymbols();
	printf("Dumping all identifiers\n");
    for(int i = 0; i < currentSize; i++) {
        free(Col_ST[i]->strList);
        printf("Free strList\n");
        free(Col_ST[i]->valueType);
        printf("Free valueType\n");
        free(Col_ST[i]->storeType);
        printf("Free storeType\n");
        free(Col_ST[i]->value);
        printf("Free value\n");
        free(Col_ST[i]->labels);
        printf("Free labels\n");
        free(Col_ST[i]->arrayAtr);
        printf("Free arrayAtr\n");
        free(Col_ST[i]->funcAtr);
        printf("Free funcAtr\n");
        free(Col_ST[i]);
        printf("Free Col_ST INDEX %d\n", i);
    }
    free(Col_ST);
}

void printSymbols() {
    printf("======================================================\n");
	printf("\nCurrent Table is  %d:\n", currentTable);
	for(int i = 0; i < currentSize; i++) {
	    printf("\nSymbol Table %d:\n", i);
        printf("Index\tStoreType\tValueType\tIDname\n");
        for(int j = 0; j < Col_ST[i]->size; j++) {
            // printf("%d\t%d\t\t%d\t\t%s\n", j, Col_ST[i]->storeType[j], Col_ST[i]->valueType[j], Col_ST[i]->strList[j]);
            printf("%d\t", j);
            switch((int)Col_ST[i]->storeType[j]) {
                case NONE:
                    printf("NONE\t\t");
                    break;
                case CONSTANT:
                    printf("CONST\t\t");
                    break;
                case VARIABLE:
                    printf("VAR\t\t");
                    break;
                case MULTIVAR:
                    printf("ARRAY\t\t");
                    break;
                case FUNC:
                    printf("FUNC\t\t");
                    break;
                case PROC:
                    printf("PROC\t\t");
                    break;
            }
            switch((int)Col_ST[i]->valueType[j]) {
                case VALNONE:
                    printf("NONE\t\t");
                    break;
                case VALINT:
                    printf("INT\t\t");
                    break;
                case VALREAL:
                    printf("REAL\t\t");
                    break;
                case VALSTR:
                    printf("STR\t\t");
                    break;
                case VALBOOL:
                    printf("BOOL\t\t");
                    break;
            }
            if((int)Col_ST[i]->storeType[j] == CONSTANT) {
                switch((int)Col_ST[i]->valueType[j]) {
                    case VALNONE:
                        printf("NONE\t\t");
                        break;
                    case VALINT:
                        printf("%d\t\t", Col_ST[i]->value[j].integer);
                        break;
                    case VALREAL:
                       printf("%lf\t\t", Col_ST[i]->value[j].real);
                        break;
                    case VALSTR:
                        printf("%s\t\t", Col_ST[i]->value[j].string);
                        break;
                    case VALBOOL:
                        printf("%d\t\t", Col_ST[i]->value[j].boolean);
                        break;
                }
            } else {
                printf("NtCONST\t\t");
            }
            printf("%s\n", Col_ST[i]->strList[j]);
        }
    }
    printf("======================================================\n\n");
}

void printChosenSymbols(int index) {
	printf("\nSymbol Table %d:\n", index);
    printf("==================\n");
    printf("Index\tStoreType\tValueType\tIDname\n");
	for(int i = 0; i < Col_ST[index]->size; i++) {
		printf("%d\t%d\t\t%d\t\t%s\n", i, Col_ST[index]->storeType[i], Col_ST[index]->valueType[i], Col_ST[index]->strList[i]);
    }
    printf("==================\n\n");
}

void printSymbolInformationIn() {
    printf("entering block, next number %d\n", currentLabel);
    labelCounter = 0;
}

void printSymbolInformationOut() {
    printf("leaving block, symbol entries:\n");
    // print table
    for(int i = 0; i < Col_ST[currentTable]->size; i++) {
        if(Col_ST[currentTable]->storeType[i] == VARIABLE) {
            printf("<\"%s\", variable,", Col_ST[currentTable]->strList[i]);
            switch((int)Col_ST[currentTable]->valueType[i]) {
                case VALINT:
                    printf("integer, ");
                    break;
                case VALBOOL:
                    printf("boolean, ");
                    break;
            }
            printf("%d>\n", Col_ST[currentTable]->labels[i]);
        }
    }
    // currentLabel = currentLabel - labelCounter;
}

void resetFunctionGrammarFlags() {
    funcCalling = false;
    funcDeclaration = false;
    currentParamIndex = 0;
    // functionParent = NULL;
    atParentIndex = -1;
    scopeReturnType = -1;
    funcScope = false;
    procScope = false;
    isCallingArray = false;
}

void initFlags() {
    funcScope = false;
    procScope = false;
    needConstExp = false;
    funcDeclaration = false;
    funcCalling = false;
    currentParamIndex = 0;
    atParentIndex = -1;
    functionParent = NULL;
    scopeReturnType = -1;
    foundIndex = -1;
    topTerm = -1;
    topFuncCalling = -1;
    isCallingArray = false;
    currentLabel = 0;
    labelCounter = 0;
    isWritingLocalVar = false;
    isWritingStmt = false;
    lCounter = 1;
    tabs = 0;
    lStackPointer = -1;
    forVarPointer = -1;
    forDecrPointer = -1;
    inMain = false;
}

int yyerror(char *s) {
   fprintf(stderr, "(Line No:%d) %s\n", yylineno, s);   
   return 0; 
}

void transGlobalVariable(char* varName, struct MultiValue data) {
    outTabs();
    fprintf(outFile, "field static ");
    switch(data.type) {
        case (int)VALINT:
            fprintf(outFile, "int %s", varName);
            if(data.hasValue) 
                fprintf(outFile, " = %d", data.value.integer);
            break;
        case (int)VALREAL:
            fprintf(outFile, "int %s", varName);
            if(data.hasValue) 
                fprintf(outFile, " = %lf", data.value.real);
            break;
        case (int)VALSTR:
            fprintf(outFile, "string %s", varName);
            if(data.hasValue) 
                fprintf(outFile, " = %s", data.value.string);
            break;
        case (int)VALBOOL:
            fprintf(outFile, "boolean %s", varName);
            if(data.hasValue) 
                fprintf(outFile, " = %d", data.value.boolean);
            break;
        default:
            yyerror("Translation for global variable failed");
            break;
    }
    fprintf(outFile, "\n");
} 

void transLocalVariable(char* varName, struct MultiValue data) {
    printf("%s = %d, next number %d\n", varName, currentLabel, currentLabel+1);

    // if(data.hasValue) {
    //     switch(data.type) {
    //         case (int)VALINT:
    //             fprintf(outFile, "sipush %d\n", data.value.integer);
    //             fprintf(outFile, "istore %d\n", currentLabel);
    //             break;
    //         case (int)VALBOOL:
    //             if(data.value.boolean)
    //                 fprintf(outFile, "iconst_1");
    //             else 
    //                 fprintf(outFile, "iconst_0");
    //             fprintf(outFile, "istore %d", currentLabel);
    //             break;
    //         default:
    //             yyerror("Translation for local variable failed");
    //             break;
    //     }
    // }

    if(data.hasValue) {
        outTabs();  
        fprintf(outFile, "istore %d\n", currentLabel);
    }

    currentLabel++;
    labelCounter++;
} 

void transExprTerm(struct MultiValue data) {
    outTabs();
    switch(data.type) {
        case (int)VALINT:
            fprintf(outFile, "sipush %d\n", data.value.integer);
            break;
        case (int)VALSTR:
            fprintf(outFile, "ldc \"%s\"\n", data.value.string);
            break;
        case (int)VALBOOL:
            if(data.value.boolean)
                fprintf(outFile, "iconst_1\n");
            else 
                fprintf(outFile, "iconst_0\n");
            break;
        default:
            yyerror("Translation for local variable failed");
            break;
    }
}

void outTabs() {
    for(int i = 0; i < tabs; i++) {
        fprintf(outFile, "\t");
    }
}

void printLine(int lineNum, char* lineText) {
    // lineText[strlen(lineText) - 1] = '\0';
	fprintf(outFile, "/* line %d: %s */\n", lineNum, lineText);
}

int main(int argc, char** argv)
{
    if (argc > 0) {
        yyin = fopen(argv[1], "r");/* open input file */
        // yyin = stdin;
    } else {
        yyin = stdin;
    }

    filename = argv[1];
    int nameLen = strlen(filename);
    filename[nameLen - 1] = '\0';
    filename[nameLen - 2] = '\0';
    filename[nameLen - 3] = '\0';

    // init output file
    char outFilename[100] = "\0";
    strcat(outFilename, filename);
    strcat(outFilename, ".jasm");
    outFile = fopen(outFilename, "w");
    if(outFile == NULL) {
        fprintf(stderr, "Error unable to open out file");
        fclose(outFile);
        return 1;   
    }

    initFlags();

    /* perform parsing */
    create();
    yyparse();
    dump();

    fclose(outFile);

    return 0;
}

