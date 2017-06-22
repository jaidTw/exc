%{
#include "y.tab.h"
#include <string>

using namespace std;

extern "C" {
    int yylex(void);
}

extern int lineno;
extern YYSTYPE yylval;
extern const char N_TYPE;
extern const char I_TYPE;
extern const char F_TYPE;
extern const char S_TYPE;
extern void *lookup(std::string name, char type);
extern void *lookup(std::string name, char type=0x0);
%}

%%

[Pp][Rr][Oo][Gg][Rr][Aa][Mm] { return PROGRAM; }
[Bb][Ee][Gg][Ii][Nn] { return BEGIN_; }
[Ee][Nn][Dd] { return END; }
[Ii][Ff] { return IF; }
[Tt][Hh][Ee][Nn] { return THEN; }
[Ee][Ll][Ss][Ee] { return ELSE; }
[Ee][Nn][Dd][Ii][Ff] { return ENDIF; }
[Ff][Oo][Rr] { return FOR; }
[Tt][Oo] { return TO; }
[Dd][Oo][Ww][Nn][Tt][Oo] { return DOWNTO; }
[Ee][Nn][Dd][Ff][Oo][Rr] { return ENDFOR; }
[Ss][Tt][Ee][Pp] { return STEP; }
[Ww][Hh][Ii][Ll][Ee] { return WHILE; }
[Ee][Nn][Dd][Ww][Hh][Ii][Ll][Ee] { return ENDWHILE; }
[Dd][Ee][Cc][Ll][Aa][Rr][Ee] { return DECLARE; }
[Aa][Ss] { return AS; }
[Ii][Nn][Tt][Ee][Gg][Ee][Rr] { return INTEGER; }
[Ff][Ll][Oo][Aa][Tt] { return FLOAT; }
":=" { return ASSIGN; }
">=" { return GE; }
"<=" { return LE; }
">" { return G; }
"<" { return L; }
"==" { return EQ; }
"!=" { return NE; }
\"[^"\n]*\" { puts("STRLIT"); return STRLIT; }
[a-zA-Z][0-9a-zA-Z_]* { yylval.symp = lookup(yytext); return ID; }
0|-?[1-9][0-9]* { yylval.symp = lookup(yytext, I_TYPE); return INTLIT; }
-?(((0|[1-9][0-9]*)\.)|([0-9]*\.[0-9]+)) { yylval.symp = lookup(yytext, F_TYPE); return FLOATLIT; }
[\t\r ]+ ;
\%\%.* ;
\n { ++lineno; }
. { return *yytext; }
%%
