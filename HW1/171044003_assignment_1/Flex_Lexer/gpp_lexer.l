%{
	#include "gpp_lexer.h"
%}

%%
[aA][nN][dD]								return KW_AND;
[oO][rR]										return KW_OR;
[nN][oO][tT]								return KW_NOT;
[eE][qQ][uU][aA][lL] 				return KW_EQUAL;
[lL][eE][sS][sS]						return KW_LESS;
[nN][iI][lL]								return KW_NIL;
[lL][iI][sS][tT]						return KW_LIST;
[aA][pP][pP][eE][nN][dD]		return KW_APPEND;
[cC][oO][nN][cC][aA][tT]		return KW_CONCAT;
[sS][eE][tT]								return KW_SET;
[dD][eE][fF][fF][uU][nN]		return KW_DEFFUN;
[fF][oO][rR]								return KW_FOR;
[iI][fF]										return KW_IF;
[eE][xX][iI][tT]						return KW_EXIT;
[lL][oO][aA][dD]						return KW_LOAD;
[dD][iI][sS][pP]						return KW_DISP;
[tT][rR][uU][eE]						return KW_TRUE;
[fF][aA][lL][sS][eE]				return KW_FALSE;

"+"												  return OP_PLUS;
"-"													return OP_MINUS;
"/"													return OP_DIV;
"*"													return OP_MULT;
"("													return OP_OP;
")"													return OP_CP;
"**"												return OP_DBLMULT;
"“"													return OP_OC;
"”"													return OP_CC;
","													return OP_COMMA;

[;][;] 											return COMMENT;

[a-zA-Z][a-zA-Z0-9]*			return IDENTIFIER;

[0]|[1-9][0-9]*							return VALUE;

[\n]												return NEWLINECHAR;
[ ]													return SPACE;

";"  												return UNKNOWN;
"!" 												return UNKNOWN;
"#" 												return UNKNOWN;
"$" 												return UNKNOWN;
"%" 												return UNKNOWN;
"&" 												return UNKNOWN;
"'"											 		return UNKNOWN;
"." 												return UNKNOWN;
":" 												return UNKNOWN;
"<" 												return UNKNOWN;
"=" 												return UNKNOWN;
">" 												return UNKNOWN;
"?" 												return UNKNOWN;
"@" 												return UNKNOWN;
"^" 												return UNKNOWN;
"`" 												return UNKNOWN;
"[" 												return UNKNOWN;
"]" 												return UNKNOWN;
"|" 												return UNKNOWN;
"{" 												return UNKNOWN;
"}" 												return UNKNOWN;
"~" 												return UNKNOWN;
"_" 												return UNKNOWN;

%%

int yywrap(void){
	return 1;
}

extern int yylex();
extern int yylineno;
extern char* yytext;

void helperMethod(FILE* input, FILE* output);

int main(int argc,char* argv[])
{
			FILE *out_file = fopen("parsed_cpp.txt", "w");
			FILE *in_file;
			if(out_file == NULL){
						printf("output file (%s) can not found!\n","parsed_cpp.txt");
			}
			else{
						// if there is no arguman, I take inputs from user.
						if(argc == 1){
									yyin = stdin;
									helperMethod(yyin,out_file);
						}
						// if there is a arguman, I read the file.
						else if(argc >= 2){
									in_file = fopen(argv[1], "r");
									if(in_file == NULL)
												printf("input file(%s) can not found!\n", argv[1]);
			    				else{
												yyin = in_file;
												helperMethod(yyin,out_file);
									}
						}
			}
      return 0;
}

void helperMethod(FILE* input, FILE* output){
			yyin = input;
			int currentToken,prevToken;
			int flag = 0; // for control the value is starting with 0 or not.
			char token[][15] = { "", "KW_AND", "KW_OR", "KW_NOT", "KW_EQUAL", "KW_LESS", "KW_NIL", "KW_LIST", "KW_APPEND", "KW_CONCAT", "KW_SET", "KW_DEFFUN", "KW_FOR",
												"KW_IF", "KW_EXIT", "KW_LOAD", "KW_DISP", "KW_TRUE", "KW_FALSE", "OP_PLUS", "OP_MINUS", "OP_DIV", "OP_MULT", "OP_OP", "OP_CP","OP_DBLMULT",
												"OP_OC", "OP_CC", "OP_COMMA", "COMMENT", "VALUE", "IDENTIFIER","SPACE","NEWLINECHAR","UNKNOWN"};

			// newline = 1 -> It means I won't read the rest of the line.
			// newline = 0 -> it means I'm on the new line now, so I have to read it.
			int newline = 0;
			do
			{
						 prevToken = currentToken;
						 currentToken = yylex();

						 if(yytext[0] == '0'){
									 		printf("ERROR\t\t: can not be such value. value can not be start with 0.\n");
							 				fprintf(output, "ERROR\t\t\t\t: can not be such value. value can not be start with 0.\n");
											flag = 1;
						 }

						 // If the comments comment and move on to the next line, the newline character is set to 1 so that it doesn't read anything.
						 else if(currentToken == COMMENT){
									printf("COMMENT\n");
									fprintf(output,"COMMENT\n");
									newline = 1;
						 }

						 else if(prevToken == NEWLINECHAR && currentToken == NEWLINECHAR){
									return;
						 }
						 //If it crosses the newline, it sets the character newline to 0.
						 else if(currentToken == NEWLINECHAR){
									newline = 0;
						 }

						// if it will continue to read the line;
						 if(newline == 0){
									 //  There cannot be value at the beginning of identifier.
									if(currentToken == IDENTIFIER && prevToken == VALUE){
												 printf("ERROR\t\t: can not be such identifier. Id can not be start with value.\n");
												 fprintf(output, "ERROR\t\t\t\t: can not be such identifier. Id can not be start with value.\n");
												 flag = 0;
									}

									else if(currentToken == OP_CP && prevToken == VALUE ){
												printf("%s\n%s\n",token[prevToken],token[currentToken]);
												fprintf(output,"%s\n%s\n",token[prevToken],token[currentToken]);
												flag = 0;
									}

									else if(currentToken == UNKNOWN){
												printf("ERROR\t\t: Unknown character: %s\n",yytext);
												fprintf(output,"ERROR\t\t\t\t: Unknown character: %s\n",yytext);
												flag = 0;
									}

									else if((currentToken == SPACE || currentToken == OP_OP || currentToken == NEWLINECHAR || currentToken == UNKNOWN) && (prevToken == VALUE)){
												if(flag == 0){
													printf("VALUE\n");
													fprintf(output, "VALUE\n");
												}
												flag = 0;
									}

									// if currentToken is not newline, space or UNKNOWN it prints the token value.
									else if(currentToken != VALUE && currentToken != NEWLINECHAR && currentToken != SPACE && currentToken != UNKNOWN){
												printf("%s\n",token[currentToken]);
												fprintf(output, "%s\n",token[currentToken]);
												flag = 0;
									}
						 }
			}while(currentToken);
}
