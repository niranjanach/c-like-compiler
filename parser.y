%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int lineNo = 1;
char* curLine = "";
char* scalar_decl_buffer = NULL;

void yyerror(char *msg);
int yylex(void);

char* concat(const char* s1, const char* s2) {
    if (s1 == NULL) s1 = "";
    if (s2 == NULL) s2 = "";
    char* result = (char*)malloc(strlen(s1) + strlen(s2) + 1);
    if (result == NULL) {
        fprintf(stderr, "Memory allocation error\n");
        exit(1);
    }
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

char* remove_expr(const char* s1) {
    const char* startTag = "<expr>";
    const char* endTag = "</expr>";
    size_t startTagLen = strlen(startTag);
    size_t endTagLen = strlen(endTag);
    
    char* result = (char*)malloc(strlen(s1) + 1);
    if (result == NULL) {
        return NULL; // Memory allocation failed
    }
    
    const char* src = s1;
    char* dst = result;

    while (*src) {
        if (strncmp(src, startTag, startTagLen) == 0) {
            src += startTagLen;
        } else if (strncmp(src, endTag, endTagLen) == 0) {
            src += endTagLen;
        } else {
            *dst++ = *src++;
        }
    }
    
    *dst = '\0';
    
    return result;
}

char* concat3(const char* s1, const char* s2, const char* s3) {
    if (s1 == NULL) s1 = "";
    if (s2 == NULL) s2 = "";
    if (s3 == NULL) s3 = "";
    char* result = (char*)malloc(strlen(s1) + strlen(s2) + strlen(s3) + 1);
    if (result == NULL) {
        fprintf(stderr, "Memory allocation error\n");
        exit(1);
    }
    strcpy(result, s1);
    strcat(result, s2);
    strcat(result, s3);
    return result;
}

char* int_to_string(int num) {
    char *str;
    int len;

    len = snprintf(NULL, 0, "%d", num);

    str = (char *)malloc(len + 1);

    if (str == NULL) {
        return NULL;
    }

    snprintf(str, len + 1, "%d", num);

    return str;
}

char* float_to_string(float num) {
    char *str;
    int len;

    len = snprintf(NULL, 0, "%f", num);

    str = (char *)malloc(len + 1);

    if (str == NULL) {
        return NULL;
    }

    snprintf(str, len + 1, "%f", num);

    return str;
}

void print(const char* s1, const char* s2){
    printf("$$ %s -> %s$.\n", s2, s1);
}

void add_to_buffer(char** buffer, const char* str) {
    if (*buffer == NULL) {
        *buffer = strdup(str);
    } else {
        *buffer = concat(*buffer, str);
    }
}

%}

%union {
    char *string_val;
    int integer_val;
    float float_val;
}

%token <integer_val> INTEGER_LITERAL 
%token <float_val> FLOAT_LITERAL
%token <string_val> TYPE IDENTIFIER  CHAR_LITERAL STRING_LITERAL NULL_LITERAL CHAR
%token <string_val> CONST SIGNED UNSIGNED LONG_LONG LONG SHORT INT FLOAT DOUBLE VOID INCREMENT DECREMENT

%token <string_val> IF ELSE SWITCH CASE DEFAULT WHILE DO FOR RETURN BREAK CONTINUE

%left TYPE
%left IDENTIFIER
%left ','
%left LOGICAL_OR
%left LOGICAL_AND
%left '|'
%left '^'
%left '&'
%left EQUAL
%left LESS_THAN LESS_THAN_OR_EQUAL GREATER_THAN GREATER_THAN_OR_EQUAL
%right '='
%left SHIFT_LEFT SHIFT_RIGHT
%left '+' '-'
%left '*' '/' '%'
%right UNARY_PLUS UNARY_MINUS '!' '~'
%right INCREMENT DECREMENT
%left '(' ')'
%left '[' ']'

%type <string_val> program idents arrays global_declaration scalar_declaration array_declaration function_declaration function_definition compound_stmt stmt_list stmt switch_clauses switch_clause var_declaration parameter_list expr type parameter_list_opt stmt_list_opt expr_opt matched_stmt unmatched_stmt array_list argument_list expr_list nested_expr_list

%%
program: /* empty */ { $$ = strdup(""); }
       | program global_declaration { $$ = concat($1, $2); }
       ;

global_declaration: scalar_declaration ';' { add_to_buffer(&scalar_decl_buffer, $1); $$ = ""; }
                   | array_declaration ';' { $$ = $1; add_to_buffer(&scalar_decl_buffer, $1); $$ = "";}
                   | function_declaration ';' { $$ = $1; add_to_buffer(&scalar_decl_buffer, $1); $$ = "";}
                   | function_definition { $$ = $1; add_to_buffer(&scalar_decl_buffer, $1); $$ = "";}
                   ;

scalar_declaration: type idents { $$ = concat3("<scalar_decl>", $1, concat($2, ";</scalar_decl>")); }
                  | type '*' idents { $$ = concat3("<scalar_decl>", $1, concat3("*", $3, ";</scalar_decl>")); }
                  ;

array_declaration: type arrays { $$ = concat3("<array_decl>", $1, concat($2, ";</array_decl>")); }
                 | type '*' arrays { $$ = concat3("<array_decl>", $1, concat3("*", $3, ";</array_decl>")); }
                 ;

arrays: IDENTIFIER array_list { $$ = concat($1, $2); }
      | IDENTIFIER array_list '=' '{' nested_expr_list '}' { $$ = concat3($1, concat($2, "="), concat3("{", $5, "}")); }
      | arrays ',' IDENTIFIER array_list { $$ = concat3($1, ",", concat($3, $4)); }
      | arrays ',' IDENTIFIER array_list '=' '{' nested_expr_list '}' { $$ = concat3(concat($1, ","), concat3($3, $4, "="), concat3("{", $7, "}")); }
      ;

array_list: '[' expr ']' { $$ = concat3("[", $2, "]"); }
          | array_list '[' expr ']' { $$ = concat3($1, "[", concat($3, "]")); }
          ;

nested_expr_list: expr { $$ = $1; }
                | '{' expr_list '}' { $$ = concat3("{", $2, "}"); }
                | nested_expr_list ',' expr { $$ = concat($1, concat(",", $3)); }
                | nested_expr_list ',' '{' expr_list '}' { $$ = concat($1, concat(",", concat3("{", $4, "}"))); }
                ;

expr_list: expr { $$ = $1; }
         | expr_list ',' expr { $$ = concat($1, concat(",", $3)); }
         ;

function_declaration: type IDENTIFIER '(' parameter_list_opt ')' { $$ = concat3("<func_decl>", concat($1, concat($2, concat("(", concat($4, ");")))), "</func_decl>"); }
                    | type '*' IDENTIFIER '(' parameter_list_opt ')' { $$ = concat3("<func_decl>", concat($1, concat("*", concat($3, concat("(", concat($5, ");"))))), "</func_decl>"); }
                    ;

parameter_list_opt: /* empty */ { $$ = strdup(""); }
                  | parameter_list { $$ = $1; }
                  ;

function_definition: type IDENTIFIER '(' parameter_list_opt ')' compound_stmt { $$ = concat3("<func_def>", concat($1, concat($2, concat("(", concat($4, concat(")", $6))))), "</func_def>"); }
                   | type '*' IDENTIFIER '(' parameter_list_opt ')' compound_stmt  { $$ = concat3("<func_def>", concat($1, concat("*", concat($3, concat("(", concat($5, concat(")", $7)))))), "</func_def>"); }

parameter_list: type IDENTIFIER { $$ = concat($1, $2); }
              | type '*' IDENTIFIER { $$ = concat3($1, "*", $3); }  
              | parameter_list ',' type IDENTIFIER { $$ = concat($1, concat(",", concat($3, $4)));  }
              ;

compound_stmt: '{' '}' { $$ = strdup("{}"); }
              | '{' stmt_list '}' { $$ = concat("{", concat($2, "}")); }
              | '{' var_declaration stmt_list_opt '}' { $$ = concat("{", concat($2, concat($3, "}"))); }
              ;

stmt_list_opt: /* empty */ { $$ = strdup(""); }
             | stmt_list { $$ = $1; }
             ;

stmt_list: stmt { $$ = concat3("<stmt>", $1, "</stmt>"); }
         | stmt_list stmt { $$ = concat($1, concat3("<stmt>", $2, "</stmt>")); }
         ;

stmt: matched_stmt 
    | unmatched_stmt ;

matched_stmt: expr ';' { $$ = concat($1, ";"); }
            | IF '(' expr ')' matched_stmt ELSE matched_stmt { $$ = concat("if(", concat($3, concat(")", concat(concat3("<stmt>", $5, "</stmt>"), concat("else", $7))))); }
            | SWITCH '(' expr ')' '{' switch_clauses '}' { $$ = concat("switch(", concat($3, concat("){", concat($6, "}")))); }
            | WHILE '(' expr ')' matched_stmt { $$ = concat("while(", concat($3, concat(")", concat3("<stmt>", $5, "</stmt>")))); }
            | DO matched_stmt WHILE '(' expr ')' ';' { $$ = concat("do", concat(concat3("<stmt>", $2, "</stmt>"), concat("while(", concat($5, ");")))); }
            | FOR '(' expr_opt ';' expr_opt ';' expr_opt ')' matched_stmt { $$ = concat("for(", concat($3, concat(";", concat($5, concat(";", concat($7, concat(")",concat3("<stmt>", $9, "</stmt>")))))))); }
            | RETURN ';' { $$ = strdup("return;"); }
            | RETURN expr ';' { $$ = concat("return", concat($2, ";")); }
            | BREAK ';' { $$ = strdup("break;"); }
            | CONTINUE ';' { $$ = strdup("continue;"); }
            | compound_stmt { $$ = $1; }
            ;

unmatched_stmt: IF '(' expr ')' stmt { $$ = concat("if(", concat($3, concat(")", $5))); }
              | IF '(' expr ')' matched_stmt ELSE unmatched_stmt { $$ = concat("if(", concat($3, concat(")", concat($5, concat("else", $7))))); }
              ;

expr_opt: /* empty */ { $$ = strdup(""); }
        | expr { $$ = $1; }
        ;

switch_clauses: /* empty */ { $$ = strdup(""); }
              | switch_clauses switch_clause { $$ = concat($1, $2); }
              ;

switch_clause: CASE INTEGER_LITERAL ':' stmt_list { $$ = concat("case", concat(int_to_string($2), concat(":", $4))); }
             | DEFAULT ':' stmt_list { $$ = concat("default:", $3); }
             ;

var_declaration: scalar_declaration ';' { $$ = $1;  }
               | var_declaration scalar_declaration ';' { $$ = concat($1, $2);  }
               | var_declaration array_declaration ';' { $$ = concat($1, $2); }
               ;

expr: IDENTIFIER { $$ = concat3("<expr>", $1, "</expr>"); }
    | IDENTIFIER '=' expr { $$ = concat3("<expr>", concat(concat3("<expr>", $1, "</expr>"), concat("=", $3)), "</expr>"); }
    | INTEGER_LITERAL { $$ = concat3("<expr>", int_to_string($1), "</expr>"); }
    | FLOAT_LITERAL { $$ = concat3("<expr>", float_to_string($1), "</expr>"); }
    | CHAR_LITERAL { $$ = concat3("<expr>", $1, "</expr>");  }
    | STRING_LITERAL { $$ = concat3("<expr>", $1, "</expr>"); }
    | NULL_LITERAL { $$ = concat3("<expr>", "0", "</expr>"); }
    | '(' expr ')' { $$ = concat3("<expr>", concat("(", concat($2, ")")), "</expr>"); }
    | '(' expr ',' expr ')' { $$ = concat3("<expr>", concat3("(", concat3($2, ",", $4), ")"), "</expr>"); }
    | '(' IDENTIFIER ',' expr ')' { $$ = concat3("<expr>", concat3("(", concat3($2, ",", $4), ")"), "</expr>"); }
    | '(' type ')' { $$ = concat3("<expr>", concat("(", concat($2, ")")), "</expr>"); }
    | '(' type ')' expr { $$ = concat3("<expr>", concat("(", concat3($2, ")", $4)), "</expr>"); }
    | expr '+' expr { $$ = concat3("<expr>", concat($1, concat("+", $3)), "</expr>"); }
    | expr '-' expr { $$ = concat3("<expr>", concat($1, concat("-", $3)), "</expr>"); }
    | expr '*' expr { $$ = concat3("<expr>", concat($1, concat("*", $3)), "</expr>"); }
    | expr '/' expr { $$ = concat3("<expr>", concat($1, concat("/", $3)), "</expr>"); }
    | expr '%' expr { $$ = concat3("<expr>", concat($1, concat("%", $3)), "</expr>"); }
    | expr LESS_THAN expr { $$ = concat3("<expr>", concat($1, concat("<", $3)), "</expr>"); }
    | expr LESS_THAN_OR_EQUAL expr { $$ = concat3("<expr>", concat($1, concat("<=", $3)), "</expr>"); }
    | expr GREATER_THAN expr { $$ = concat3("<expr>", concat($1, concat(">", $3)), "</expr>"); }
    | expr GREATER_THAN_OR_EQUAL expr { $$ = concat3("<expr>", concat($1, concat(">=", $3)), "</expr>"); }
    | expr EQUAL expr { $$ = concat3("<expr>", concat($1, concat("==", $3)), "</expr>"); }
    | expr '!' EQUAL expr { $$ = concat3("<expr>", concat($1, concat("!=", $4)), "</expr>"); }
    | expr LOGICAL_AND expr { $$ = concat3("<expr>", concat($1, concat("&&", $3)), "</expr>"); }
    | expr LOGICAL_OR expr { $$ = concat3("<expr>", concat($1, concat("||", $3)), "</expr>"); }
    | '!' expr { $$ = concat3("<expr>", concat("!", $2), "</expr>"); }
    | expr '&' expr { $$ = concat3("<expr>", concat($1, concat("&", $3)), "</expr>"); }
    | expr '|' expr { $$ = concat3("<expr>", concat($1, concat("|", $3)), "</expr>"); }
    | expr '^' expr { $$ = concat3("<expr>", concat($1, concat("^", $3)), "</expr>"); }
    | '~' expr { $$ = concat3("<expr>", concat("~", $2), "</expr>"); }
    | expr SHIFT_LEFT expr { $$ = concat3("<expr>", concat($1, concat("<expr><<</expr>", $3)), "</expr>"); }
    | expr SHIFT_RIGHT expr { $$ = concat3("<expr>", concat($1, concat("<expr>>></expr>", $3)), "</expr>"); }
    | IDENTIFIER '(' argument_list ')' { $$ = concat3("<expr>", concat(concat3("<expr>",$1,"</expr>"), concat("(", concat($3, ")"))), "</expr>"); }
    | IDENTIFIER '(' argument_list ',' IDENTIFIER array_list ')' { $$ = concat3("<expr>", concat(concat3("<expr>",$1,"</expr>"), concat("(", concat3($3, ",", concat3("<expr>",concat3($5,$6,"</expr>"),")")))), "</expr>"); }
    | IDENTIFIER '(' argument_list ',' IDENTIFIER ')' { $$ = concat3("<expr>", concat($1, concat("(", concat3($3, ",", concat($5, ")")))), "</expr>"); }
    | IDENTIFIER '(' ')' { $$ = concat3("<expr>",concat(concat3("<expr>",$1,"</expr>"),"()"), "</expr>"); }
    | IDENTIFIER INCREMENT { $$ = concat3("<expr>", concat($1, concat3("<expr>",$2,"</expr>")), "</expr>"); }
    | IDENTIFIER DECREMENT { $$ = concat3("<expr>", concat($1, concat3("<expr>",$2,"</expr>")), "</expr>"); }
    | INCREMENT IDENTIFIER { $$ = concat3("<expr>", concat(concat3("<expr>",$1,"</expr>"), $2), "</expr>"); }
    | DECREMENT IDENTIFIER { $$ = concat3("<expr>", concat(concat3("<expr>",$1,"</expr>"), $2), "</expr>"); }
    | '+' expr %prec UNARY_PLUS { $$ = concat3("<expr>", concat("+", $2), "</expr>"); }
    | '-' expr %prec UNARY_MINUS { $$ = concat3("<expr>", concat("-", $2), "</expr>"); }
    | '*' expr %prec UNARY_PLUS { $$ = concat3("<expr>", concat("*", $2), "</expr>"); }
    | '&' expr %prec UNARY_PLUS { $$ = concat3("<expr>", concat("&", $2), "</expr>"); }
    ;

argument_list: expr { $$ = $1; }
             | argument_list ',' expr { $$ = concat($1, concat(",", $3)); }          
             ;

type: TYPE { $$ = $1; }
    | CONST TYPE { $$ = concat("const", $2); }
    | CONST SIGNED TYPE { $$ = concat("constsigned", $3); }
    | CONST UNSIGNED TYPE { $$ = concat("constunsigned", $3); }
    | CONST SIGNED LONG_LONG { $$ = concat("constsignedlonglong", ""); }
    | CONST UNSIGNED LONG_LONG { $$ = concat("constunsignedlonglong", ""); }
    | CONST SIGNED LONG { $$ = concat("constsignedlong", ""); }
    | CONST UNSIGNED LONG { $$ = concat("constunsignedlong", ""); }
    | CONST SIGNED SHORT { $$ = concat("constsignedshort", ""); }
    | CONST UNSIGNED SHORT { $$ = concat("constunsignedshort", ""); }
    | SIGNED TYPE { $$ = concat("signed", $2); }
    | UNSIGNED TYPE { $$ = concat("unsigned", $2); }
    | SIGNED LONG_LONG { $$ = concat("signedlonglong", ""); }
    | UNSIGNED LONG_LONG { $$ = concat("unsignedlonglong", ""); }
    | SIGNED LONG { $$ = concat("signedlong", ""); }
    | UNSIGNED LONG { $$ = concat("unsignedlong", ""); }
    | SIGNED SHORT { $$ = concat("signedshort", ""); }
    | UNSIGNED SHORT { $$ = concat("unsignedshort", ""); }
    | SIGNED CHAR { $$ = strdup("signedchar"); }
    | UNSIGNED CHAR { $$ = strdup("unsignedchar"); }
    | FLOAT { $$ = strdup("float"); }
    | CONST { $$ = strdup("const"); }
    | DOUBLE { $$ = strdup("double"); }
    | VOID { $$ = strdup("void"); }
    | SHORT { $$ = strdup("short"); }
    | LONG { $$ = strdup("long"); }
    | LONG_LONG { $$ = strdup("longlong"); }
    | INT { $$ = strdup("int"); }
    ;

idents: IDENTIFIER { $$ = $1; }
      | idents ',' expr { $$ = concat($1, concat(",", remove_expr($3))); }
      | IDENTIFIER '=' expr { $$ = concat($1, concat("=", $3)); }
      ;

%%
void yyerror(char *msg) {
    fprintf(stderr, "Error at line %d: %s\n msg : %s\n", lineNo, curLine, msg);
    exit(1);
}

int main() {
    yyparse();
    if (scalar_decl_buffer != NULL) {
        printf("%s", scalar_decl_buffer);
    }
    return 0;
}

