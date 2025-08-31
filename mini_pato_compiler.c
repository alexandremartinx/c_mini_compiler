// mini_compiler_pato.c — Compilador da linguagem Pato (.pato) -> x86-64 assembly (Intel syntax)
// Regras principais:
//   - quack expr;                // imprime inteiro
//   - quack "texto";             // imprime string literal
//   - let x = expr;              // declaração
//   - x = expr;                  // atribuição
//   - let a[N];                  // array global de inteiros (64-bit)
//   - a[i] = expr;  x = a[i];    // indexação (somente arrays globais)
//   - fn nome(p1, p2, ...) { ... return expr; }
//   - if (cond) stmt [else stmt]
//   - while (cond) stmt
//   - { bloco } e escopo com sombra
//
// Limites e notas:
//   - Inteiros 64-bit. Sem floats.
//   - Arrays apenas GLOBAIS nesta versão (locais exigem gerenciamento extra da pilha).
//   - Strings apenas literais: podem ser passadas ao 'quack' (impressão textual).
//   - Convenção SysV: rdi,rsi,rdx,rcx,r8,r9; retorno em rax. Pilha alinhada.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define MAX_SRC   (1<<20)
#define MAX_TOK   (1<<20)
#define MAX_NAME  128
#define MAX_VARS  4096
#define MAX_STRL  (1<<20)

// ---------- Tokens ----------
typedef enum {
    T_EOF=0, T_LET, T_QUACK, T_IF, T_ELSE, T_WHILE, T_FN, T_RETURN,
    T_NAME, T_NUM, T_STRING,
    T_PLUS, T_MINUS, T_STAR, T_SLASH,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,
    T_LBRACK, T_RBRACK,
    T_EQ, T_SEMI, T_COMMA,
    T_EQEQ, T_NEQ, T_LT, T_LE, T_GT, T_GE,
    T_AND, T_OR
} TokKind;

typedef struct {
    TokKind kind;
    char    lex[MAX_NAME];
    long    ival;
    int     line, col;
    int     str_id; // para literais de string
} Token;

// ---------- Globais do lexer ----------
static const char *SRC;
static int   POS, LINE=1, COL=1;
static Token TOKS[MAX_TOK];
static size_t NTOK;

// ---------- Erro ----------
static void die(const char* fmt, ...){
    va_list ap; va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fputc('\n', stderr);
    exit(1);
}

// ---------- Tabela de strings (.rodata) ----------
typedef struct { char *s; } StrLit;
static StrLit STRS[MAX_VARS];
static int    NSTR = 0;

static int add_string_literal(const char* z){
    if (NSTR >= MAX_VARS) die("too many string literals");
    STRS[NSTR].s = strdup(z);
    return NSTR++;
}

// ---------- Lexer ----------
static void skip_ws_and_comments(void){
    for(;;){
        if (!SRC[POS]) return;
        if (SRC[POS]=='/' && SRC[POS+1]=='/'){
            while(SRC[POS] && SRC[POS]!='\n'){ POS++; COL++; }
        } else if (isspace((unsigned char)SRC[POS])){
            if (SRC[POS]=='\n'){ LINE++; COL=1; POS++; }
            else { POS++; COL++; }
        } else return;
    }
}
static int is_name_start(int c){ return isalpha(c) || c=='_'; }
static int is_name_char (int c){ return isalnum(c) || c=='_'; }

static void add_tok(TokKind k, const char* lex, long ival, int sid){
    if (NTOK >= MAX_TOK) die("too many tokens");
    TOKS[NTOK].kind=k; TOKS[NTOK].ival=ival; TOKS[NTOK].line=LINE; TOKS[NTOK].col=COL; TOKS[NTOK].str_id=sid;
    if (lex){ strncpy(TOKS[NTOK].lex, lex, MAX_NAME-1); TOKS[NTOK].lex[MAX_NAME-1]='\0'; }
    else TOKS[NTOK].lex[0]='\0';
    NTOK++;
}

static int hexv(char c){
    if (c>='0'&&c<='9') return c-'0';
    if (c>='a'&&c<='f') return 10+(c-'a');
    if (c>='A'&&c<='F') return 10+(c-'A');
    return -1;
}

static void lex_string(void){
    POS++; COL++; // skip "
    char buf[MAX_STRL]; int bi=0;
    while(SRC[POS] && SRC[POS]!='"'){
        char c = SRC[POS++];
        COL++;
        if (c=='\\'){
            char e = SRC[POS++]; COL++;
            switch(e){
                case 'n': buf[bi++]='\n'; break;
                case 't': buf[bi++]='\t'; break;
                case 'r': buf[bi++]='\r'; break;
                case '\\': buf[bi++]='\\'; break;
                case '"': buf[bi++]='"'; break;
                case 'x': {
                    int h1=hexv(SRC[POS++]), h2=hexv(SRC[POS++]); COL+=2;
                    if (h1<0||h2<0) die("invalid \\x escape");
                    buf[bi++]=(char)((h1<<4)|h2);
                } break;
                default: buf[bi++]=e; break;
            }
        } else {
            buf[bi++]=c;
        }
        if (bi>=MAX_STRL-1) die("string literal too long");
    }
    if (SRC[POS]!='"') die("unterminated string");
    POS++; COL++;
    buf[bi]=0;
    int sid = add_string_literal(buf);
    add_tok(T_STRING,NULL,0,sid);
}

static void lex_all(void){
    while(1){
        skip_ws_and_comments();
        int c = SRC[POS];
        if (!c){ add_tok(T_EOF,NULL,0,-1); return; }

        if (c=='"'){ lex_string(); continue; }

        if (is_name_start(c)){
            int st=POS; POS++; COL++;
            while(is_name_char((unsigned char)SRC[POS])){ POS++; COL++; }
            int len=POS-st; if (len>=MAX_NAME) die("identifier too long");
            char buf[MAX_NAME]; memcpy(buf, SRC+st, len); buf[len]='\0';
            if (!strcmp(buf,"let"))   add_tok(T_LET,buf,0,-1);
            else if (!strcmp(buf,"quack")) add_tok(T_QUACK,buf,0,-1);
            else if (!strcmp(buf,"if")) add_tok(T_IF,buf,0,-1);
            else if (!strcmp(buf,"else")) add_tok(T_ELSE,buf,0,-1);
            else if (!strcmp(buf,"while")) add_tok(T_WHILE,buf,0,-1);
            else if (!strcmp(buf,"fn")) add_tok(T_FN,buf,0,-1);
            else if (!strcmp(buf,"return")) add_tok(T_RETURN,buf,0,-1);
            else add_tok(T_NAME,buf,0,-1);
            continue;
        }
        if (isdigit((unsigned char)c)){
            long v=0;
            while(isdigit((unsigned char)SRC[POS])){ v=v*10+(SRC[POS]-'0'); POS++; COL++; }
            add_tok(T_NUM,NULL,v,-1); continue;
        }
        // dois chars
        if (c=='=' && SRC[POS+1]=='='){ add_tok(T_EQEQ,"==",0,-1); POS+=2; COL+=2; continue; }
        if (c=='!' && SRC[POS+1]=='='){ add_tok(T_NEQ,"!=",0,-1); POS+=2; COL+=2; continue; }
        if (c=='<' && SRC[POS+1]=='='){ add_tok(T_LE,"<=",0,-1); POS+=2; COL+=2; continue; }
        if (c=='>' && SRC[POS+1]=='='){ add_tok(T_GE,">=",0,-1); POS+=2; COL+=2; continue; }
        if (c=='&' && SRC[POS+1]=='&'){ add_tok(T_AND,"&&",0,-1); POS+=2; COL+=2; continue; }
        if (c=='|' && SRC[POS+1]=='|'){ add_tok(T_OR,"||",0,-1); POS+=2; COL+=2; continue; }
        // um char
        switch(c){
            case '+': add_tok(T_PLUS,"+",0,-1); POS++; COL++; break;
            case '-': add_tok(T_MINUS,"-",0,-1); POS++; COL++; break;
            case '*': add_tok(T_STAR,"*",0,-1); POS++; COL++; break;
            case '/': add_tok(T_SLASH,"/",0,-1); POS++; COL++; break;
            case '(': add_tok(T_LPAREN,"(",0,-1); POS++; COL++; break;
            case ')': add_tok(T_RPAREN,")",0,-1); POS++; COL++; break;
            case '{': add_tok(T_LBRACE,"{",0,-1); POS++; COL++; break;
            case '}': add_tok(T_RBRACE,"}",0,-1); POS++; COL++; break;
            case '[': add_tok(T_LBRACK,"[",0,-1); POS++; COL++; break;
            case ']': add_tok(T_RBRACK,"]",0,-1); POS++; COL++; break;
            case '=': add_tok(T_EQ,"=",0,-1); POS++; COL++; break;
            case ';': add_tok(T_SEMI,";",0,-1); POS++; COL++; break;
            case ',': add_tok(T_COMMA,",",0,-1); POS++; COL++; break;
            case '<': add_tok(T_LT,"<",0,-1); POS++; COL++; break;
            case '>': add_tok(T_GT,">",0,-1); POS++; COL++; break;
            default: die("Lex error (%d:%d): invalid char '%c'", LINE, COL, c);
        }
    }
}

// ---------- Parser base ----------
typedef struct { size_t i; } Parser;
static Token* pk(Parser* p){ return &TOKS[p->i]; }
static Token* gt(Parser* p){ return &TOKS[p->i++]; }
static int accept(Parser* p, TokKind k){ if (pk(p)->kind==k){ p->i++; return 1; } return 0; }
static void expect(Parser* p, TokKind k, const char* msg){
    if (!accept(p,k)){ Token* t=pk(p); die("Parse error (%d:%d): expected %s, got kind=%d", t->line,t->col,msg,t->kind); }
}

// ---------- String builder ----------
typedef struct { char *buf; size_t cap,len; } Str;
static void s_init(Str* s){ s->cap=8192; s->len=0; s->buf=(char*)malloc(s->cap); s->buf[0]='\0'; }
static void s_puts(Str* s, const char* t){
    size_t n=strlen(t);
    if (s->len+n+1>s->cap){ while(s->len+n+1>s->cap) s->cap*=2; s->buf=(char*)realloc(s->buf,s->cap); }
    memcpy(s->buf+s->len,t,n+1); s->len+=n;
}
static void s_printf(Str* s, const char* fmt, ...){
    va_list ap; va_start(ap, fmt);
    char tmp[4096]; int n=vsnprintf(tmp,sizeof tmp,fmt,ap); va_end(ap);
    if (n<0) return; if ((size_t)n>=sizeof tmp){ tmp[sizeof tmp-2]='\n'; tmp[sizeof tmp-1]='\0'; n=(int)strlen(tmp); }
    s_puts(s,tmp);
}

// ---------- Globais / Arrays ----------
typedef enum { GV_SCALAR, GV_ARRAY } GVarKind;
typedef struct { char name[MAX_NAME]; GVarKind kind; long arr_len; } GVar;
static GVar GLOB[MAX_VARS];
static int  NGLOB=0;

static int find_gvar(const char* nm){ for(int i=0;i<NGLOB;i++) if(!strcmp(GLOB[i].name,nm)) return i; return -1; }
static void ensure_gvar_scalar(const char* nm){
    int i=find_gvar(nm);
    if (i>=0){
        if (GLOB[i].kind!=GV_SCALAR) die("'%s' already declared as array", nm);
        return;
    }
    if (NGLOB>=MAX_VARS) die("too many globals");
    strncpy(GLOB[NGLOB].name,nm,MAX_NAME-1); GLOB[NGLOB].name[MAX_NAME-1]='\0';
    GLOB[NGLOB].kind=GV_SCALAR; GLOB[NGLOB].arr_len=0; NGLOB++;
}
static void ensure_gvar_array(const char* nm, long len){
    if (find_gvar(nm)>=0) die("global '%s' re-declared", nm);
    if (len<=0) die("array '%s' must have positive length", nm);
    if (NGLOB>=MAX_VARS) die("too many globals");
    strncpy(GLOB[NGLOB].name,nm,MAX_NAME-1); GLOB[NGLOB].name[MAX_NAME-1]='\0';
    GLOB[NGLOB].kind=GV_ARRAY; GLOB[NGLOB].arr_len=len; NGLOB++;
}

// ---------- Locais / Escopo ----------
typedef struct { char name[MAX_NAME]; int offset; int level; } Local;
static Local LOCALS[MAX_VARS];
static int   NLOC=0, LSTACK=0, LEVEL=0, IN_FN=0;
static char  CUR_FN[MAX_NAME];

static int find_local_idx(const char* nm){ for(int i=NLOC-1;i>=0;i--) if(!strcmp(LOCALS[i].name,nm)) return i; return -1; }
static int add_local(const char* nm){
    if (NLOC>=MAX_VARS) die("too many locals");
    LSTACK += 8;
    strncpy(LOCALS[NLOC].name,nm,MAX_NAME-1); LOCALS[NLOC].name[MAX_NAME-1]='\0';
    LOCALS[NLOC].offset=LSTACK; LOCALS[NLOC].level=LEVEL; NLOC++;
    return LSTACK;
}
static void enter_block(void){ LEVEL++; }
static void leave_block(void){ while(NLOC>0 && LOCALS[NLOC-1].level==LEVEL){ NLOC--; } LEVEL--; }
static void reset_fn_ctx(void){ NLOC=0; LSTACK=0; LEVEL=0; IN_FN=0; CUR_FN[0]='\0'; }

// ---------- Labels e alinhamento de call ----------
static int next_lbl(void){ static int id=1; return id++; }
static int expr_depth=0;
static void push_reg(Str* b, const char* r){ s_printf(b,"    push %s\n", r); expr_depth++; }
static void pop_to (Str* b, const char* r){ s_printf(b,"    pop %s\n",  r); expr_depth--; }
static void discard_top(Str* b){ s_puts(b,"    add rsp, 8\n"); expr_depth--; }
static void call_aligned(Str* b, const char* tgt){
    if ((expr_depth % 2)==0){ s_puts(b,"    sub rsp, 8\n"); s_printf(b,"    call %s\n", tgt); s_puts(b,"    add rsp, 8\n"); }
    else s_printf(b,"    call %s\n", tgt);
}

// ---------- Forward ----------
static void gen_expr_push(struct Parser* p, Str* b);

// ---------- Var load/store ----------
static void load_var_push(Str* b, const char* nm){
    if (IN_FN){
        int i=find_local_idx(nm);
        if (i>=0){ s_printf(b,"    mov rax, qword ptr [rbp - %d]\n", LOCALS[i].offset); push_reg(b,"rax"); return; }
    }
    int g=find_gvar(nm);
    if (g<0) die("Use of undeclared variable '%s'", nm);
    if (GLOB[g].kind!=GV_SCALAR) die("'%s' is an array (need index)", nm);
    s_printf(b,"    mov rax, qword ptr [rip + %s]\n", nm); push_reg(b,"rax");
}
static void store_var_from_stack(Str* b, const char* nm){
    if (IN_FN){
        int i=find_local_idx(nm);
        if (i>=0){ pop_to(b,"rax"); s_printf(b,"    mov qword ptr [rbp - %d], rax\n", LOCALS[i].offset); return; }
    }
    int g=find_gvar(nm);
    if (g<0) die("Assignment to undeclared global '%s'", nm);
    if (GLOB[g].kind!=GV_SCALAR) die("'%s' is an array (need index)", nm);
    pop_to(b,"rax"); s_printf(b,"    mov qword ptr [rip + %s], rax\n", nm);
}

// ---------- Arrays globais ----------
static void array_addr_global_push(Str* b, const char* nm, Parser* p){
    int g=find_gvar(nm);
    if (g<0 || GLOB[g].kind!=GV_ARRAY) die("Undeclared or non-array '%s'", nm);
    gen_expr_push(p,b);       // index
    pop_to(b,"rax");          // rax = i
    s_printf(b,"    lea rbx, [rip + %s]\n", nm); // base
    s_puts(b,"    imul rax, 8\n");               // escala (int64)
    s_puts(b,"    add rax, rbx\n");
    push_reg(b,"rax");        // &a[i]
}
static void array_store_from_stack(Str* b){ // [..., &a[i], val]
    pop_to(b,"rbx"); // val
    pop_to(b,"rax"); // addr
    s_puts(b,"    mov qword ptr [rax], rbx\n");
}
static void array_load_push(Str* b){ // [..., &a[i]] -> load
    pop_to(b,"rax");
    s_puts(b,"    mov rax, qword ptr [rax]\n"); push_reg(b,"rax");
}

// ---------- Chamadas de função ----------
static void gen_call_push(Parser* p, Str* b, const char* fname){
    const char* regs[6]={"rdi","rsi","rdx","rcx","r8","r9"};
    int n=0;
    if (!accept(p,T_RPAREN)){
        do{ gen_expr_push(p,b); n++; if (n>6) die("functions support up to 6 args"); } while(accept(p,T_COMMA));
        expect(p,T_RPAREN,"')'");
    }
    for(int i=n-1;i>=0;i--) pop_to(b, regs[i]);
    s_puts(b,"    xor eax, eax\n");
    call_aligned(b, fname);
    push_reg(b,"rax");
}

// ---------- Expressões ----------
static void gen_factor_push(Parser* p, Str* b);
static void gen_unary_push (Parser* p, Str* b);
static void gen_term_push  (Parser* p, Str* b);
static void gen_add_push   (Parser* p, Str* b);
static void gen_rel_push   (Parser* p, Str* b);
static void gen_eq_push    (Parser* p, Str* b);
static void gen_and_push   (Parser* p, Str* b);
static void gen_or_push    (Parser* p, Str* b);

static void gen_factor_push(Parser* p, Str* b){
    Token* t=pk(p);
    if (t->kind==T_NUM){ long v=t->ival; gt(p); s_printf(b,"    mov rax, %ld\n", v); push_reg(b,"rax"); return; }
    if (t->kind==T_STRING){ int sid=t->str_id; gt(p); s_printf(b,"    lea rax, [rip + .Lstr%d]\n", sid); push_reg(b,"rax"); return; }
    if (t->kind==T_NAME){
        char nm[MAX_NAME]; strncpy(nm,t->lex,MAX_NAME); gt(p);
        if (accept(p,T_LPAREN)){ gen_call_push(p,b,nm); return; }
        if (accept(p,T_LBRACK)){ array_addr_global_push(b,nm,p); expect(p,T_RBRACK,"']'"); array_load_push(b); return; }
        load_var_push(b,nm); return;
    }
    if (accept(p,T_LPAREN)){ gen_expr_push(p,b); expect(p,T_RPAREN,"')'"); return; }
    die("Parse error (%d:%d): bad factor", t->line,t->col);
}
static void gen_unary_push(Parser* p, Str* b){
    if (accept(p,T_MINUS)){ gen_unary_push(p,b); pop_to(b,"rax"); s_puts(b,"    neg rax\n"); push_reg(b,"rax"); }
    else gen_factor_push(p,b);
}
static void gen_term_push(Parser* p, Str* b){
    gen_unary_push(p,b);
    for(;;){
        if (accept(p,T_STAR)){ gen_unary_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    imul rax, rbx\n"); push_reg(b,"rax"); }
        else if (accept(p,T_SLASH)){ gen_unary_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    cqo\n    idiv rbx\n"); push_reg(b,"rax"); }
        else break;
    }
}
static void gen_add_push(Parser* p, Str* b){
    gen_term_push(p,b);
    for(;;){
        if (accept(p,T_PLUS)){ gen_term_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    add rax, rbx\n"); push_reg(b,"rax"); }
        else if (accept(p,T_MINUS)){ gen_term_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    sub rax, rbx\n"); push_reg(b,"rax"); }
        else break;
    }
}
static void gen_rel_push(Parser* p, Str* b){
    gen_add_push(p,b);
    for(;;){
        if (accept(p,T_LT)){ gen_add_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    cmp rax, rbx\n    setl al\n    movzx rax, al\n"); push_reg(b,"rax"); }
        else if (accept(p,T_LE)){ gen_add_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    cmp rax, rbx\n    setle al\n    movzx rax, al\n"); push_reg(b,"rax"); }
        else if (accept(p,T_GT)){ gen_add_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    cmp rax, rbx\n    setg al\n    movzx rax, al\n"); push_reg(b,"rax"); }
        else if (accept(p,T_GE)){ gen_add_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    cmp rax, rbx\n    setge al\n    movzx rax, al\n"); push_reg(b,"rax"); }
        else break;
    }
}
static void gen_eq_push(Parser* p, Str* b){
    gen_rel_push(p,b);
    for(;;){
        if (accept(p,T_EQEQ)){ gen_rel_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    cmp rax, rbx\n    sete al\n    movzx rax, al\n"); push_reg(b,"rax"); }
        else if (accept(p,T_NEQ)){ gen_rel_push(p,b); pop_to(b,"rbx"); pop_to(b,"rax"); s_puts(b,"    cmp rax, rbx\n    setne al\n    movzx rax, al\n"); push_reg(b,"rax"); }
        else break;
    }
}
static void gen_and_push(Parser* p, Str* b){
    gen_eq_push(p,b);
    while(accept(p,T_AND)){
        int Lf=next_lbl(), Le=next_lbl();
        pop_to(b,"rax"); s_puts(b,"    cmp rax, 0\n"); s_printf(b,"    je .L%d\n", Lf);
        gen_eq_push(p,b);
        pop_to(b,"rax"); s_puts(b,"    cmp rax, 0\n"); s_printf(b,"    je .L%d\n", Lf);
        s_puts(b,"    mov rax, 1\n"); s_printf(b,"    jmp .L%d\n", Le);
        s_printf(b,".L%d:\n", Lf); s_puts(b,"    xor eax, eax\n");
        s_printf(b,".L%d:\n", Le); push_reg(b,"rax");
    }
}
static void gen_or_push(Parser* p, Str* b){
    gen_and_push(p,b);
    while(accept(p,T_OR)){
        int Lt=next_lbl(), Le=next_lbl();
        pop_to(b,"rax"); s_puts(b,"    cmp rax, 0\n"); s_printf(b,"    jne .L%d\n", Lt);
        gen_and_push(p,b);
        pop_to(b,"rax"); s_puts(b,"    cmp rax, 0\n"); s_printf(b,"    jne .L%d\n", Lt);
        s_puts(b,"    xor eax, eax\n"); s_printf(b,"    jmp .L%d\n", Le);
        s_printf(b,".L%d:\n", Lt); s_puts(b,"    mov rax, 1\n");
        s_printf(b,".L%d:\n", Le); push_reg(b,"rax");
    }
}
static void gen_expr_push(Parser* p, Str* b){ gen_or_push(p,b); }

// ---------- Statements ----------
static void gen_stmt(Parser* p, Str* b); // fwd

static void gen_block(Parser* p, Str* b){
    if (accept(p,T_LBRACE)){
        enter_block();
        while(pk(p)->kind!=T_RBRACE) gen_stmt(p,b);
        expect(p,T_RBRACE,"'}'");
        leave_block();
    } else gen_stmt(p,b);
}

// quack: aceita inteiro geral OU literal de string direto
static void gen_quack(Parser* p, Str* b){
    // caso especial: literal de string imediatamente
    if (pk(p)->kind==T_STRING){
        int sid = pk(p)->str_id; gt(p);
        expect(p,T_SEMI,"';'");
        s_printf(b,"    lea rsi, [rip + .Lstr%d]\n", sid);
        s_puts(b,"    lea rdi, [rip + fmt_s]\n    xor eax, eax\n");
        call_aligned(b,"printf");
        return;
    }
    // geral: avalia expr como inteiro
    gen_expr_push(p,b); expect(p,T_SEMI,"';'");
    pop_to(b,"rax");
    s_puts(b,"    mov rsi, rax\n    lea rdi, [rip + fmt_i]\n    xor eax, eax\n");
    call_aligned(b,"printf");
}

static void gen_if(Parser* p, Str* b){
    expect(p,T_LPAREN,"'('"); gen_expr_push(p,b); expect(p,T_RPAREN,"')'");
    int Lelse=next_lbl(), Lend=next_lbl();
    pop_to(b,"rax"); s_puts(b,"    cmp rax, 0\n"); s_printf(b,"    je .L%d\n", Lelse);
    gen_block(p,b);
    s_printf(b,"    jmp .L%d\n", Lend);
    s_printf(b,".L%d:\n", Lelse);
    if (accept(p,T_ELSE)) gen_block(p,b);
    s_printf(b,".L%d:\n", Lend);
}

static void gen_while(Parser* p, Str* b){
    int Lb=next_lbl(), Le=next_lbl();
    s_printf(b,".L%d:\n", Lb);
    expect(p,T_LPAREN,"'('"); gen_expr_push(p,b); expect(p,T_RPAREN,"')'");
    pop_to(b,"rax"); s_puts(b,"    cmp rax, 0\n"); s_printf(b,"    je .L%d\n", Le);
    gen_block(p,b);
    s_printf(b,"    jmp .L%d\n", Lb);
    s_printf(b,".L%d:\n", Le);
}

static void gen_decl_let(Parser* p, Str* b){
    Token* id=pk(p);
    if (id->kind!=T_NAME) die("expected name after 'let'");
    char nm[MAX_NAME]; strncpy(nm,id->lex,MAX_NAME); gt(p);

    if (accept(p,T_LBRACK)){
        // array global apenas
        if (IN_FN) die("local arrays not implemented (use global)");
        Token* t=pk(p); if (t->kind!=T_NUM) die("array size must be integer literal");
        long len=t->ival; gt(p);
        expect(p,T_RBRACK,"']'"); expect(p,T_SEMI,"';'");
        ensure_gvar_array(nm,len);
        return;
    }

    expect(p,T_EQ,"'='");
    if (IN_FN){
        add_local(nm);
        gen_expr_push(p,b); expect(p,T_SEMI,"';'");
        pop_to(b,"rax"); int li=find_local_idx(nm);
        s_printf(b,"    mov qword ptr [rbp - %d], rax\n", LOCALS[li].offset);
    } else {
        ensure_gvar_scalar(nm);
        gen_expr_push(p,b); expect(p,T_SEMI,"';'");
        pop_to(b,"rax"); s_printf(b,"    mov qword ptr [rip + %s], rax\n", nm);
    }
}

static void gen_assign(Parser* p, Str* b, const char* nm){
    if (IN_FN){
        int li=find_local_idx(nm), gi=find_gvar(nm);
        if (li<0 && gi<0) die("Assignment to undeclared '%s'", nm);
        gen_expr_push(p,b); expect(p,T_SEMI,"';'");
        if (li>=0){ pop_to(b,"rax"); s_printf(b,"    mov qword ptr [rbp - %d], rax\n", LOCALS[li].offset); }
        else { if (GLOB[gi].kind!=GV_SCALAR) die("'%s' is an array (need index)", nm);
               pop_to(b,"rax"); s_printf(b,"    mov qword ptr [rip + %s], rax\n", nm); }
    } else {
        int gi=find_gvar(nm); if (gi<0) die("Assignment to undeclared global '%s'", nm);
        if (GLOB[gi].kind!=GV_SCALAR) die("'%s' is an array (need index)", nm);
        gen_expr_push(p,b); expect(p,T_SEMI,"';'");
        pop_to(b,"rax"); s_printf(b,"    mov qword ptr [rip + %s], rax\n", nm);
    }
}

static void gen_array_assign(Parser* p, Str* b, const char* nm){
    if (find_gvar(nm)<0 || GLOB[find_gvar(nm)].kind!=GV_ARRAY) die("'%s' not a declared array", nm);
    array_addr_global_push(b,nm,p);
    expect(p,T_RBRACK,"']'"); expect(p,T_EQ,"'='");
    gen_expr_push(p,b); expect(p,T_SEMI,"';'");
    array_store_from_stack(b);
}

static void gen_return(Parser* p, Str* b){
    if (!IN_FN) die("return outside function");
    gen_expr_push(p,b); expect(p,T_SEMI,"';'");
    pop_to(b,"rax"); s_printf(b,"    jmp .Lret_%s\n", CUR_FN);
}

static void gen_expr_stmt(Parser* p, Str* b){
    gen_expr_push(p,b); expect(p,T_SEMI,"';'"); discard_top(b);
}

static void gen_stmt(Parser* p, Str* b){
    if (accept(p,T_SEMI)) return;
    if (accept(p,T_LET))   { gen_decl_let(p,b); return; }
    if (accept(p,T_QUACK)) { gen_quack(p,b);    return; }
    if (accept(p,T_IF))    { gen_if(p,b);       return; }
    if (accept(p,T_WHILE)) { gen_while(p,b);    return; }
    if (accept(p,T_RETURN)){ gen_return(p,b);   return; }

    if (pk(p)->kind==T_NAME){
        Token name = *pk(p); gt(p);
        if (accept(p,T_LBRACK)){ gen_array_assign(p,b,name.lex); return; }
        if (accept(p,T_EQ))    { gen_assign(p,b,name.lex);       return; }
        p->i--; // volta: é expressão (ex: chamada de função)
    }
    if (pk(p)->kind==T_LBRACE){ gen_block(p,b); return; }

    gen_expr_stmt(p,b);
}

// ---------- Funções / Programa ----------
static void gen_function(Parser* p, Str* out_funcs){
    Token* id=pk(p); if (id->kind!=T_NAME) die("expected function name");
    strncpy(CUR_FN,id->lex,MAX_NAME); CUR_FN[MAX_NAME-1]='\0'; gt(p);
    expect(p,T_LPAREN,"'('");

    reset_fn_ctx(); IN_FN=1; LEVEL=0; expr_depth=0;

    const char* regs[6]={"rdi","rsi","rdx","rcx","r8","r9"};
    Str prm; s_init(&prm);
    int n=0;
    if (!accept(p,T_RPAREN)){
        do{
            Token* t=pk(p); if (t->kind!=T_NAME) die("expected param name");
            char nm[MAX_NAME]; strncpy(nm,t->lex,MAX_NAME); gt(p);
            if (n>=6) die("max 6 params");
            int off=add_local(nm);
            s_printf(&prm,"    mov qword ptr [rbp - %d], %s\n", off, regs[n]);
            n++;
        } while(accept(p,T_COMMA));
        expect(p,T_RPAREN,"')'");
    }
    expect(p,T_LBRACE,"'{'");

    Str body; s_init(&body);
    enter_block();
    while(pk(p)->kind!=T_RBRACE) gen_stmt(p,&body);
    expect(p,T_RBRACE,"'}'");
    leave_block();

    s_printf(out_funcs,".text\n.global %s\n%s:\n", CUR_FN, CUR_FN);
    s_puts(out_funcs,"    push rbp\n    mov rbp, rsp\n");
    if (LSTACK>0) s_printf(out_funcs,"    sub rsp, %d\n", LSTACK);
    s_puts(out_funcs, prm.buf);
    s_puts(out_funcs, body.buf);
    s_printf(out_funcs, ".Lret_%s:\n", CUR_FN);
    if (LSTACK>0) s_printf(out_funcs,"    add rsp, %d\n", LSTACK);
    s_puts(out_funcs,"    pop rbp\n    ret\n");

    free(prm.buf); free(body.buf);
    reset_fn_ctx();
}

static void gen_toplevel(Parser* p, Str* out_funcs, Str* out_main){
    IN_FN=0; LEVEL=0; expr_depth=0;
    while(pk(p)->kind!=T_EOF){
        if (accept(p,T_FN)) gen_function(p,out_funcs);
        else gen_stmt(p,out_main);
    }
}

// ---------- main ----------
int main(void){
    static char buf[MAX_SRC];
    size_t n=fread(buf,1,MAX_SRC-1, stdin);
    buf[n]='\0';
    SRC=buf; POS=0; LINE=1; COL=1;

    lex_all();

    Parser p={0};
    Str text_funcs; s_init(&text_funcs);
    Str main_body;  s_init(&main_body);
    gen_toplevel(&p,&text_funcs,&main_body);

    Str out; s_init(&out);
    // rodata: formatos e strings
    s_puts(&out,
        ".intel_syntax noprefix\n"
        ".extern printf\n"
        ".section .rodata\n"
        "fmt_i: .asciz \"%ld\\n\"\n"
        "fmt_s: .asciz \"%s\\n\"\n"
    );
    for(int i=0;i<NSTR;i++){
        s_printf(&out, ".Lstr%d: .asciz \"", i);
        for (const unsigned char* pch=(unsigned char*)STRS[i].s; *pch; ++pch){
            unsigned char c=*pch;
            if (c=='\"' || c=='\\') s_printf(&out, "\\%c", c);
            else if (c=='\n') s_puts(&out, "\\n");
            else if (c=='\t') s_puts(&out, "\\t");
            else if (c=='\r') s_puts(&out, "\\r");
            else if (c<32 || c>=127) s_printf(&out, "\\x%02X", c);
            else s_printf(&out, "%c", c);
        }
        s_puts(&out, "\"\n");
    }

    // funções
    s_puts(&out, text_funcs.buf);

    // main
    s_puts(&out,
        ".text\n.global main\n"
        "main:\n"
        "    push rbp\n"
        "    mov rbp, rsp\n"
    );
    s_puts(&out, main_body.buf);
    s_puts(&out,
        "    xor eax, eax\n"
        "    pop rbp\n"
        "    ret\n"
    );

    // globais
    s_puts(&out, ".section .bss\n.align 8\n");
    for(int i=0;i<NGLOB;i++){
        if (GLOB[i].kind==GV_SCALAR){
            s_printf(&out, "%s: .zero 8\n", GLOB[i].name);
        } else {
            long bytes = GLOB[i].arr_len * 8;
            s_printf(&out, "%s: .zero %ld\n", GLOB[i].name, bytes);
        }
    }

    fwrite(out.buf,1,out.len,stdout);

    for(int i=0;i<NSTR;i++) free(STRS[i].s);
    free(text_funcs.buf); free(main_body.buf); free(out.buf);
    return 0;
}
