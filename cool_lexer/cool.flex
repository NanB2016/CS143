/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno = 1;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int nested_level = 0;
bool max_str_check();
void max_str_error();

%}

%x COMMENT DASHCOMMENT STRING

/*
 * Define names for regular expressions here.
 */

DARROW          =>

CLASS		(?i:class)
ELSE		(?i:else)
FI		(?i:fi)
IF		(?i:if)
IN		(?i:in)
INHERITS	(?i:inherits)
ISVOID		(?i:isvoid)
LET		(?i:let)
LOOP		(?i:loop)
POOL		(?i:pool)
THEN		(?i:then)
WHILE		(?i:while)
CASE		(?i:case)
ESAC		(?i:esac)
NEW		(?i:new)
OF		(?i:of)
NOT		(?i:not)
TRUE		(?-i:t)(?i:rue)
FALSE		(?-i:f)(?i:alse)

DIGIT		[0-9]
LETTER		[a-zA-Z_]
TYPEID		[:upper:]({DIGIT}|{LETTER})*
OBJECTID	[:lower:]({DIGIT}|{LETTER})*
INTEGER		{DIGIT}+

WHITESPACE	[ \f\r\t\v]	

%%

 /*
  *  Nested comments
  */

"(*"	{ BEGIN(COMMENT); nested_level++; }
"*)"	{ cool_yylval.error_msg = "Unmatched *)"; return ERROR; }
<COMMENT><<EOF>> { cool_yylval.error_msg = "EOF in comment"; BEGIN(INITIAL); return ERROR; }
<COMMENT>.	{}
<COMMENT>\n	{ curr_lineno++; }
<COMMENT>"*)"	{ nested_level--; if (nested_level==0) {BEGIN(INITIAL);} }

"--"		{ BEGIN(DASHCOMMENT); )	
<DASHCOMMENT>\n	{ curr_lineno++; BEGIN(INITIAL); }
<DASHCOMMENT><<EOF>> { curr_lineno++; BEGIN(INITIAL); }

 /*
  *  The multiple-character operators.
  */

{DARROW}		{ return (DARROW); }
"<="			{ return LE; }
"<-"			{ return ASSIGN; }

 /* The single-character operators. */

"<"			{ return '<'; }
"="			{ return '='; }
"+"			{ return '+'; }
"-" 			{ return '-'; }
"*"			{ return '*'; }
"/"			{ return '/'; } 
"~"			{ return '~'; }
"{"			{ return '{'; }
"}"			{ return '}'; }
"("			{ return '('; }
")"			{ return ')'; }
"."			{ return '.'; }
","			{ return ','; }
";"			{ return ';'; }
":"			{ return ':'; }
"@"			{ return '@'; }


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS}			{ return CLASS; }
{ELSE}			{ return ELSE; }
{FI}			{ return FI; }
{IF}			{ return IF; }
{IN} 			{ return IN; }
{INHERITS}		{ return INHERITS; }
{LET}			{ return LET; }
{LOOP}                  { return LOOP; }
{POOL}			{ return POOL; }
{THEN}			{ return THEN; }
{WHILE}			{ return WHILE; }
{CASE} 			{ return CASE; }
{ESAC}			{ return ESAC; }
{OF}			{ return OF; }
{NEW}			{ return NEW; }
{ISVOID}		{ return ISVOID; }
{NOT}			{ return NOT; }
{TRUE}			{ cool_yylval.boolean = 1; return BOOL_CONST; }
{FALSE}			{ cool_yylval.boolean = 0; return BOOL_CONST; }

{INTEGER}		{ cool_yylval.symbol = inttable.add_string(yytext); return INT_CONST; }

 /* identifiers */

{TYPEID}		{ cool_yylval.symbol = idtable.add_string(yytext); return TYPEID; }
{OBJECTID}		{ cool_yylval.symbol = idtable.add_string(yytext); return OBJECTID; }


{WHITESPACE}		{}		
\n			{ curr_lineno++; }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\"	{ string_buf_ptr = string_buf; BEGIN(STRING); }

<STRING>\"		{ if (max_str_check) return max_str_error; *string_buf_ptr = '\0\'; cool_yylval.symbol = stringtable.add_string(yytext); return STR_CONST; }

<STRING><<EOF>>		{ BEGIN(INITIAL); cool_yylval.error_msg = "EOF in string constant"; return ERROR; }
<STRING>\n		{ cool_yylval.error_msg = "Unterminated string constant"; BEGIN(INITIAL); curr_lineno++; return ERROR; }
<STRING>\0		{ cool_yyval.error_msg = "String contains null character"; return ERROR; }

<STRING>\\n		{ if (max_str_check) return max_str_error; *string_buf_ptr = yytext[0]; string_buf_ptr++; }
<STRING>\\t		{ if (max_str_check) return max_str_error; *string_buf_ptr = yytext[0]; string_buf_ptr++; }
<STRING>\\b		{ if (max_str_check) return max_str_error; *string_buf_ptr = yytext[0]; string_buf_ptr++; }
<STRING>\\f		{ if (max_str_check) return max_str_error; *string_buf_ptr = yytext[0]; string_buf_ptr++; }
<STRING>\\.		{ if (max_str_check) return max_str_error; *string_buf_ptr = yytext[1]; string_buf_ptr++; }

<STRING>.	{ if (max_str_check) return max_str_error; *string_buf_ptr = yytext[0]; string_buf_ptr++; )

.		{ cool_yylval.error_msg = yytext; return ERROR; }

%%

bool max_str_check(){
	return (string_buf_ptr - string_buf + 1 > MAX_STR_CONST);
}

void max_str_error(){
	cool_yylval.error_msg = "String constant too long";
	return ERROR;
}

