%{
#define MAX_LINE_LENGTH 256
#define LIST strcat(buf,yytext)
// #define token(s) {LIST; printf("<%s>\n", s);} 
// #define tokenIdent(t) {LIST; printf("<identifier:%s>\n", t);} 
// #define tokenNum(t) {LIST; printf("<num:%s>\n", t);}
#define token(s) {LIST; } 
#define tokenIdent(t) {LIST; } 
#define tokenNum(t) {LIST; }
#include<stdio.h>
#include<string.h>
#include<ctype.h>
#include<stdbool.h>
#include "y.tab.h"

// char* uppercase(char* s);
int linenum = 1;
char buf[MAX_LINE_LENGTH];
char finalBuf[MAX_LINE_LENGTH];
char strBuf[MAX_LINE_LENGTH];
void create();
int lookup(char* s);
int insert(char* s);
void dump();
void printSymbols();
void printLine(int lineNum, char* lineText);
%}

DIGITS [0-9]+
LETTER [a-zA-Z]
ID {LETTER}({LETTER}|{DIGITS})*

%Start com multiCom multiEnd string stringOne stringTwo stringThree
%%


\% {
	// LIST;
	BEGIN com;}
	
<com>.* {
	// LIST; 
	BEGIN 0;}

<multiCom>\n {
	// LIST;
	// printf("%d: %s", linenum, buf);
	linenum++; 
	yylineno++;
	buf[0] = '\0';
	BEGIN multiCom; }

<multiCom>(\*+)\} {
	// LIST;
	BEGIN 0; }



<multiCom>[^%}\n]+ {
	// LIST;
	BEGIN multiCom; }

\{\% {
	// LIST;
	BEGIN multiCom; }

<stringOne>\"\" {
	LIST;
	printf("\"");
	strcat(strBuf, "\"");
	BEGIN string; }
	
<stringOne>\" {
	LIST;
	printf(">\n");
	BEGIN 0; 
	yylval.sval = strBuf;
	return STRING; }

\" {
	LIST;
	printf("<string:");
	strBuf[0] = '\0';
	BEGIN string; }
	
<string>[^\"]*/\" {
	LIST;
	printf("%s", yytext);
	strcat(strBuf, yytext);
	BEGIN stringOne; }

"." { token("."); return DOT; }
"," { token(","); return COMMA; }
":" { token(":"); return COLON; }
";" { token(";"); return SEMICOLON; }
"(" { token("("); return POPEN; }
")" { token(")"); return PCLOSE; }
"[" { token("["); return SOPEN; }
"]" { token("]"); return SCLOSE; }
"{" { token("{"); return BOPEN; }
"}" { token("}"); return BCLOSE; }

"+" 			{ token("+"); return ADD; }
"-" 			{ token("-"); return SUB; }
"*" 			{ token("*"); return MUL; }
"/" 			{ token("/"); return DIV; }
"mod"/.*		{ token("mod"); return MOD; }
":=" 			{ token(":="); return ASSIGN; }
"<" 			{ token("<"); return RELOPLT; }
"<=" 			{ token("<="); return RELOPLE; }
">" 			{ token(">"); return RELOPGT; }
">=" 			{ token(">="); return RELOPGE; }
"=" 			{ token("="); return RELOPEQ; }
"not=" 			{ token("not="); return RELOPNE; }
"and" 			{ token("and"); return AND; }
"or" 			{ token("or"); return OR; }
"not" 			{ token("not");  return NOT; }

"array" 			{ token("ARRAY");  return ARRAY; }
"begin" 			{ token("BEGIN"); return BEGINT; }
"bool" 				{ token("BOOL"); return BOOL; }
"char" 				{ token("CHAR"); return CHAR; }
"const" 			{ token("CONST"); return CONST; }
"decreasing"		{ token("DECREASING"); return DECREASING; }
"default"			{ token("DEFAULT"); return DEFAULT; }
"do" 				{ token("DO"); return DO; }
"else"				{ token("ELSE"); return ELSE; }
"end" 				{ token("END");  return END; }
"exit"				{ token("EXIT"); return EXIT; }
"false"				{ token("FALSE"); return FALSE; }
"for"				{ token("FOR"); return FOR; }
"function" 			{ token("FUNCTION"); return FUNCTION;}
"get"				{ token("GET"); return GET; }
"if"				{ token("IF"); return IF;}
"int" 				{ token("INT"); return INT; }
"loop" 				{ token("LOOP"); return LOOP; }
"of"				{ token("OF"); return OF;}
"put"				{ token("PUT"); return PUT; }
"procedure"			{ token("PROCEDURE"); return PROCEDURE; }
"real"				{ token("REAL"); return REAL;}
"result"			{ token("RESULT"); return RESULT;}
"return" 			{ token("RETURN"); return RETURN;}
"skip" 				{ token("SKIP"); return SKIP; }
"string" 			{ token("STRING"); return STRING;}
"then"				{ token("THEN"); return THEN; }
"true"				{ token("TRUE"); return TRUE; }
"var"				{ token("VAR"); return VAR; }
"when"				{ token("WHEN"); return WHEN; }


{DIGITS}\.{DIGITS} {
	// printf("digit\n");
	tokenNum(yytext);
	yylval.dval = atof(yytext);
	return REAL; } 

{DIGITS} {
	// printf("digit\n");
	tokenNum(yytext);
	yylval.ival = atoi(yytext);
	return INT; } 


{ID} {
	// printf("id\n");
	tokenIdent(yytext);
	int idIndex = lookup(yytext);
	if(idIndex == -1) {
		idIndex = insert(yytext);
		if(idIndex == -1) {
			printf("Fail to insert new identifier.");
		}
	}
	yylval.ival = idIndex;
	// yylval.sval = strdup(yytext);
	return ID;
	// printf("index: %d\n", idIndex); 
	} 
   

\n {
	// LIST;
	// printf("%d: %s", linenum, buf);
	printLine(linenum, buf);
	// if(buf[0] != '\0') {
	// 	strcpy(finalBuf, buf);
	// }
	yylineno++;
	linenum++; 
	buf[0] = '\0'; 
	/* Only for debug */
	// return NEWLINE;
	}


[ \t]* {
	LIST;
	}


%%

char* getLineText() {
	return finalBuf;
}

// char* append(char* s, char c) {
// 	size_t size = strlen(s);
// 	char* newStr = malloc(size + 2);
// 	strcpy(newStr, s);
// 	newStr[size] = c;
// 	newStr[size + 1] = '\0';
	
// 	return newStr;
// }

// char* uppercase(char* s) {
// 	size_t size = strlen(s);
// 	char* newStr = malloc(size+1);
// 	for(int i = 0; i < (int) size; i++) {
// 		newStr[i] = toupper(s[i]);
// 	}
// 	newStr[size] = '\0';
	
// 	return newStr;
// }


// int yywrap(void){
// 	return 1;
// }

// int main(int argc, char** argv)
// {
	
	// if (argc > 0) {
    //     yyin = fopen(argv[1], "r");
    // } else {
    //     yyin = stdin;
    // }
    
//     create();
// 	yylex();
// 	dump();
	
// 	return 0;
// }
