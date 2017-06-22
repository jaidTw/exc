%{
#include <deque>
#include <vector>
#include <unordered_map>
#include <string>
#include <cstring>
#include <iostream>

using namespace std;

extern "C" {
    extern int yylex(void);
    void yyerror(const char *s);
}

typedef pair<string, char> symbol;
extern constexpr char N_TYPE = 0x0;
extern constexpr char I_TYPE = 0x1;
extern constexpr char F_TYPE = 0x2;
extern constexpr char S_TYPE = 0x3;

deque<string> IR;
unordered_map<string, char> SYMTAB;

void *lookup(string name, char type);
void *lookup(string name, char type = N_TYPE);

int lineno = 0;
int lbl_counter = 0;
int tmp_counter = 0;

inline string name(void *symp) { return get<0>(*(symbol *)symp); }
inline char type(void *symp) { return get<1>(*(symbol *)symp); }
inline void dumpsym(void *s) { cout << "NAME " << name(s) << " TYPE " << int(type(s)) << endl; }
inline string lbl_name(int no) { return string("lb&") + std::to_string(no); }
inline string tmp_name(int no) { return string("T&") + std::to_string(no); }
inline string new_lbl() { return lbl_name(++lbl_counter); }
inline string new_tmp() { return tmp_name(++tmp_counter); }

constexpr size_t BUFFER_LEN = 512;
constexpr char OP_PLUS = 0;
constexpr char OP_MINUS = 1;
constexpr char OP_MULTIPLY = 2;
constexpr char OP_DIVISION = 3;
constexpr char OP_G = 4;
constexpr char OP_GE = 5;
constexpr char OP_L = 6;
constexpr char OP_LE = 7;
constexpr char OP_EQ = 8;
constexpr char OP_NE = 9;

void *ir_gen_negate(void *a);
void *ir_gen_infix(void *a, void *b, int op);
void *ir_gen_inc(void *a);
void *ir_gen_begin(void *p);
void *ir_gen_halt(void *p);
void *ir_gen_j(void *dst, int op);
void *ir_gen_store(void *val, void *dst);
void *ir_gen_call(void *sub, void *param, ...);
void *ir_gen_decl(string nam, void *type);
void *ir_gen_decl_arr(string nam, void *type, string size);
void ir_gen_decl_tmp();
%}

%union{
// symp is a symbol pointer points to the slot in SYMTAB
    void *symp;
    char strp[512];
    void *notused;
}

%token <notused> PROGRAM BEGIN_ END DECLARE AS IF THEN ELSE ENDIF FOR STEP TO DOWNTO ENDFOR WHILE ENDWHILE
%token <notused> ASSIGN EQ NE G GE L LE SEMICOLON
%token <symp> ID STRLIT INTLIT INTEGER FLOATLIT FLOAT
%token <notused> '+' '-' '*' '/'

%left '+' '-'
%left '*' '/'
%left UMINUS

%type <symp> var_lst var expr type lhs assign for_disp for_decl_end bool_op
%%

start : PROGRAM ID program { ir_gen_begin($2); ir_gen_halt($2); ir_gen_decl_tmp(); };

program : BEGIN_ stmt_lst END;

stmt_lst : stmt stmt_lst
         | stmt;

stmt : decl ';'
     | assign ';'
     | ID '(' params ')' ';'
     | loop
     | cond
     |;

decl : DECLARE var_lst AS type {
    char buf[BUFFER_LEN];
    strcpy(buf, name($2).c_str());
    char *tok = strtok(buf, ",");
    while(tok != NULL) {
        char *lpos, *rpos;
        if((lpos = strchr(tok, '[')) == NULL) {
            ir_gen_decl(tok, $4);
        } else {
            rpos = strchr(tok, ']');
            ir_gen_decl_arr(string(tok, lpos - tok), $4, string(lpos + 1, rpos - lpos - 1));
        }
        tok = strtok(NULL, ",");
    }
};

var_lst : var_lst ',' var { $$ = lookup(name($1) + "," + name($3)); }
        | var { $$ = $1; };

var : ID { $$ = $1; }
    | ID '[' expr ']' { $$ = lookup(name($1) + "[" + name($3) + "]"); };

type : INTEGER { $$ = lookup("Integer", I_TYPE); }
     | FLOAT { $$ = lookup("Float", F_TYPE); };

assign : lhs ASSIGN expr { $$ = ir_gen_store($3, $1); };

lhs : ID { $$ = $1; }
    | ID '[' expr ']';

params : param
       | params ',' param;

param : INTLIT
      | FLOATLIT
      | STRLIT
      | ID;

loop : FOR '(' assign for_disp expr for_decl_end stmt_lst ENDFOR {
    if(type($3) != I_TYPE)
        yyerror("FOR counter accepts only integer expressions");
    ir_gen_inc($3);
    IR.push_back("I_CMP " + name($3) + "," + name($5));
    if(name($4) == "TO")
        ir_gen_j($6, OP_L);
    if(name($4) == "DOWNTO")
        ir_gen_j($6, OP_G);
}
     | FOR '(' for_disp expr STEP expr for_decl_end stmt_lst ENDFOR;

for_decl_end : ')' {
    string lbl = new_lbl();
    IR.push_back(lbl + ":");
    $$ = lookup(lbl);
}

for_disp : TO { $$ = lookup("TO"); }
         | DOWNTO { $$ = lookup("DOWNTO"); };

cond : IF '(' bool_expr ')' THEN stmt_lst ELSE stmt_lst ENDIF;

bool_expr : expr bool_op expr;

bool_op : GE { $$ = lookup(">="); }
        | LE { $$ = lookup("<="); }
        | G { $$ = lookup(">"); }
        | L { $$ = lookup("<"); }
        | EQ { $$ = lookup("=="); }
        | NE { $$ = lookup("!="); };

expr : expr '+' expr { $$ = ir_gen_infix($1, $3, OP_PLUS); }
     | expr '-' expr { $$ = ir_gen_infix($1, $3, OP_MINUS); }
     | expr '*' expr { $$ = ir_gen_infix($1, $3, OP_MULTIPLY); }
     | expr '/' expr { $$ = ir_gen_infix($1, $3, OP_DIVISION); }
     | '-' expr %prec UMINUS{ $$ = ir_gen_negate($2); }
    | '(' expr ')' { $$ = $2; }
    | ID { $$ = $1; }
    | ID '[' expr ']' { $$ = lookup(name($1) + "[" + name($3) + "]", type($1)); }
    | INTLIT { $$ = $1; }
    | FLOATLIT { $$ = $1; }

%%

void yyerror(const char *s) {
    cerr << lineno << ":" << s << endl;
    exit(1);
}

void *lookup(string key, char type) {
    auto iter = SYMTAB.begin();
    if((iter = SYMTAB.find(key)) == SYMTAB.end()) {
        iter = get<0>(SYMTAB.insert({key, type}));
    } else {
        if(type != N_TYPE)
            SYMTAB[key] = type;
    }
    return &*iter;
}

void *ir_gen_negate(void *a) {
    string tmp = new_tmp();
    if(type(a) == I_TYPE) {
        IR.push_back("I_UMINUS " + name(a) + ", "+ tmp);
    } else if(type(a) == F_TYPE) {
        IR.push_back("F_UMINUS " + name(a) + "," + tmp);
    }
    return lookup(tmp, type(a));
}

void *ir_gen_infix(void *a, void *b, int op) {
    string opname = "";
    switch(op) {
    case OP_PLUS: opname = "ADD "; break;
    case OP_MINUS: opname = "SUB "; break;
    case OP_MULTIPLY: opname = "MUL "; break;
    case OP_DIVISION: opname = "DIV "; break;
    }
    string tmp = new_tmp();
    if(type(a) == I_TYPE && type(b) == I_TYPE) {
        IR.push_back("I_" + opname + name(a) + "," + name(b) + "," + tmp);
        return lookup(tmp, I_TYPE);
    } else if(type(a) == F_TYPE || type(b) == F_TYPE){
        IR.push_back("F_" + opname + name(a) + "," + name(b) + "," + tmp);
        return lookup(tmp, F_TYPE);
    }
    yyerror("Invalid type");
    return NULL;
}

void *ir_gen_inc(void *a) {
    IR.push_back("INC " + name(a));
    return a;
}

void *ir_gen_begin(void *p) {
    IR.push_front("START " + name(p));
    return p;
}

void *ir_gen_halt(void *p) {
    IR.push_back("HALT " + name(p));
    return p;
}

void *ir_gen_j(void *t, int op) {
    string postfix = "";
    switch(op) {
    case 0: postfix = " "; break;
    case OP_GE: postfix = "GE "; break;
    case OP_LE: postfix = "LE "; break;
    case OP_G: postfix = "G "; break;
    case OP_L: postfix = "L "; break;
    case OP_EQ: postfix = "E "; break;
    case OP_NE: postfix = "NE "; break;
    default : yyerror("Invalid jump type");
    }
    IR.push_back("J" + postfix + name(t));
    return lookup(name(t));
}

void *ir_gen_store(void *val, void *dst) {
    if(type(dst) == I_TYPE) {
        IR.push_back("I_STORE " + name(val) + "," + name(dst));
    } else if(type(dst) == F_TYPE) {
        IR.push_back("F_STORE " + name(val) + "," + name(dst));
    } else {
        yyerror("Invalid type for assignment");
    }
    return lookup(name(val), type(dst));
}

void *ir_gen_decl(string nam, void *typ) {
    IR.push_back("Declare " + nam + ", " + name(typ));
    return lookup(nam, type(typ));
}

void *ir_gen_decl_arr(string nam, void *typ, string size) {
    IR.push_back("Declare " + nam + ", "+ name(typ) + "_array, " + size);
    return lookup(nam, type(typ));
}

void ir_gen_decl_tmp() {
    for(int i = 0; i < tmp_counter; ++i) {
        string tmp_nam = tmp_name(i);
        void *tmp = lookup(tmp_nam);
        if(type(tmp) == I_TYPE)
            IR.push_back("Declare " + tmp_nam + ", Integer");
        else if(type(tmp) == F_TYPE)
            IR.push_back("Declare " + tmp_nam + ", Float");
    }
}
