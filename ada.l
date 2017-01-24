%{
int lineno = 1;
#include "y.tab.h"
%}
%%
is			{return(IS);}
type			{return(TYPE);}
of			{return(OF);}
begin			{return(BEG);}
end			{return(END);}
".."			{return(DOTDOT);}
procedure		{return(PROCEDURE);}
null			{return(NULLWORD);}
loop			{return(LOOP);}
while                   {return(WHILE);}
quote			{return(QUOTE);}
array			{return(ARRAY);}
record			{return(RECORD);}
in			{return(IN);}
out			{return(OUT);}
range			{return(RANGE);}
constant		{return(CONSTANT);}
":="			{return(ASSIGN);}
exception		{return(EXCEPTION);}
if			{return(IF);}
for                     {return(FOR);}
reverse                 {return(REVERSE);}
then			{return(THEN);}
elsif			{return(ELSEIF);}
else			{return(ELSE);}
exit			{return(EXIT);}
when			{return(WHEN);}
and			{return(AND);}
or			{return(OR);}
not			{return(NOT);}
"="			{return(EQ);}
"/="			{return(NEQ);}
">"			{return(GT);}
"<"			{return(LT);}
">="			{return(GTE);}
"<="			{return(LTE);}
"**"			{return(STAR);}
raise			{return(RAISE);}
others			{return(OTHERS);}
"=>"			{return(ARROW);}

[a-zA-Z]([a-zA-Z0-9_])* {yylval.var = (char *)malloc(strlen(yytext)+1);
                         strcpy(yylval.var, yytext); 
                         return ID;}

[0-9]+  		{yylval.integer = atoi(yytext); 
                         return(NUMBER);} /* <number> */

\n                      {lineno++;}			

[\t ]+			/* ignore spaces and tabs */ ;

"--".*			/*ignore */ ;
	
.                      { return yytext[0]; } /* other chars */
%%
yyerror(char* s) {
	printf("%d : %s at the %s symbol \n", lineno, s, yytext);
}
