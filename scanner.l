%{
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "y.tab.h"

int yylex(void);

extern YYSTYPE yylval;


%}

ch [a-z]
key for|do|while|break|continue|if|else|return|struct|switch|case|default|void|int|double|float|char|const|signed|unsigned|short|long
id [a-zA-Z_][a-zA-Z0-9_]*
op \+\+|--|<=|>=|==|!=|&&|\|\||!|&|\||[-+*\/%=><]
punc [,:;\.\[\]\(\)\{\}]
integer [-+]?[0-9]+
float [-+]?[0-9]*(\.[0-9]*)?
char \'(\\.|[^\'])\'
string \"(\\.|[^\\"])*\"

%option yywrap


%%


"NULL"                            {  return NULL_LITERAL;}
"if"                            {  return IF;}
"else"                          {  return ELSE;}
"while"                         {  return WHILE;}
"do"                            {  return DO;}
"for"                           {  return FOR;}
"return"                        {  return RETURN;}
"break"                         {  return BREAK;}
"continue"                      {  return CONTINUE;}
"const"                         {  return CONST;}
"signed"                        {  return SIGNED;}
"unsigned"                      {  return UNSIGNED;}
"case"                          {  return CASE;}
"switch"                        {  return SWITCH;}
"default"                       {  return DEFAULT;}
"long long"                     { yylval.string_val = strdup(yytext); return LONG_LONG;}
"++"                            {  yylval.string_val = strdup(yytext); return INCREMENT;}
"--"                            { yylval.string_val = strdup(yytext); return DECREMENT;}

">"                             {  yylval.string_val = strdup(yytext); return GREATER_THAN;}
"<"                             {  yylval.string_val = strdup(yytext); return LESS_THAN;}
">="                            {  yylval.string_val = strdup(yytext); return GREATER_THAN_OR_EQUAL;}
"<="                            {  yylval.string_val = strdup(yytext); return LESS_THAN_OR_EQUAL;}
"=="                            {  yylval.string_val = strdup(yytext); return EQUAL;}

">>"                             { yylval.string_val = strdup(yytext); return SHIFT_RIGHT;}
"<<"                             { yylval.string_val = strdup(yytext); return SHIFT_LEFT;}
"||"                             { yylval.string_val = strdup(yytext); return LOGICAL_OR;}
"&&"                             { yylval.string_val = strdup(yytext); return LOGICAL_AND;}

"["                             { return yytext[0];}
"]"                             { return yytext[0];}

{key}                           { yylval.string_val = strdup(yytext); return TYPE;}
{id}                            { yylval.string_val = strdup(yytext); return IDENTIFIER;}
{op}                            { yylval.string_val = strdup(yytext); return yytext[0];}
{punc}                          { yylval.string_val = strdup(yytext); return yytext[0];}
{integer}                       { yylval.integer_val = atoi(yytext); return INTEGER_LITERAL;}
{float}                         { yylval.float_val = atof(yytext); return FLOAT_LITERAL;}
{char}                          { yylval.string_val = strdup(yytext); return CHAR_LITERAL;}
{string}                        { yylval.string_val = strdup(yytext); return STRING_LITERAL;}

\n                              {}

[ \t]+                          {}

.                               {;}

%%

int yywrap() {
    return 1;
}

