// mini_compiler_asm_funcs.c — "Mini" -> x86-64 assembly (Intel syntax)
// Uso:
//   gcc mini_compiler_asm_funcs.c -o mini_compiler_asm
//   ./mini_compiler_asm < ex.mini > out.s
//   gcc out.s -o out
//   ./out
//
// Suporta:
//  - let/print; expr;  + - * / ; unário -
//  - == != < <= > >= ; && || (short-circuit)
//  - if/else ; while ; {} ; //comentários
//  - fn nome(params...) { ... return expr; } ; chamada de função f(a,b,...)
//  - locais na pilha (8 bytes por local), globais em .bss
//
// Limites: até 6 parâmetros por função (rdi,rsi,rdx,rcx,r8,r9). Sem ponto flutuante.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define MAX_SRC  (1<<20)
#define MAX_TOK  (1<<20)
#define MAX_VARS 4096
#define MAX_NAME 128

// ---------- TOKENS ----------
typedef enum {
    T_EOF=0, T_LET, T_PRINT, T_IF, T_ELSE, T_WHILE, T_FN, T_RETURN,
    T_NAME, T_NUM,
    T_PLUS, T_MINUS, T_STAR, T_SLASH,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,
    T_EQ, T_SEMI, T_COMMA,
    T_EQEQ, T_NEQ, T_LT, T_LE, T_GT, T_GE,
    T_AND, T_OR
} TokKind;

typedef struct {
    TokKind kind;
    char    lex[MAX_NAME];
    long    ival;
    int     line, col;
} Token;

static const char *src;
static int posi, linei=1, coli=1;
static Token toks[MAX_TOK];
static size_t ntok;

static void lex_error(const char* msg){
    fprintf(stderr, "Lex error (%d:%d): %s\n", linei, coli, msg);
    exit(1);
}

static void skip_ws_and_comments(void){
    for(;;){
        if (!src[posi]) return;
        if (src[posi]=='/' && src[posi+1]=='/'){
            while(src[posi] && src[posi] != '\n'){ posi++; coli++; }
        } else if (isspace((unsigned char)src[posi])){
            if (src[posi]=='\n'){ linei++; coli=1; posi++; }
            else { posi++; coli++; }
        } else return;
    }
}

static int is_name_start(int c){ return isalpha(c) || c=='_'; }
static int is_name_char (int c){ return isalnum(c) || c=='_'; }

static void add_tok(TokKind k, const char* lex, long ival){
    if (ntok>=MAX_TOK) lex_error("too many tokens");
    toks[ntok].kind = k;
    toks[ntok].ival = ival;
    toks[ntok].line = linei;
    toks[ntok].col  = coli;
    if (lex){ strncpy(toks[ntok].lex, lex, MAX_NAME-1); toks[ntok].lex[MAX_NAME-1]='\0'; }
    else toks[ntok].lex[0]='\0';
    ntok++;
}

static void lex_all(void){
    while(1){
        skip_ws_and_comments();
        int c = src[posi];
        if (!c){ add_tok(T_EOF,NULL,0); return; }

        if (is_name_start(c)){
            int start=posi;
            posi++; coli++;
            while(is_name_char((unsigned char)src[posi])){ posi++; coli++; }
            int len = posi - start;
            if (len >= MAX_NAME) lex_error("identifier too long");
            char buf[MAX_NAME]; memcpy(buf, src+start, len); buf[len]='\0';
            if (!strcmp(buf,"let"))    add_tok(T_LET, buf, 0);
            else if (!strcmp(buf,"print")) add_tok(T_PRINT, buf, 0);
            else if (!strcmp(buf,"if"))    add_tok(T_IF, buf, 0);
            else if (!strcmp(buf,"else"))  add_tok(T_ELSE, buf, 0);
            else if (!strcmp(buf,"while")) add_tok(T_WHILE, buf, 0);
            else if (!strcmp(buf,"fn"))    add_tok(T_FN, buf, 0);
            else if (!strcmp(buf,"return"))add_tok(T_RETURN, buf, 0);
            else add_tok(T_NAME, buf, 0);
            continue;
        }

        if (isdigit((unsigned char)c)){
            long v=0;
            while(isdigit((unsigned char)src[posi])){ v = v*10 + (src[posi]-'0'); posi++; coli++; }
            add_tok(T_NUM, NULL, v);
            continue;
        }

        // 2-char ops
        if (c=='=' && src[posi+1]=='='){ add_tok(T_EQEQ,"==",0); posi+=2; coli+=2; continue; }
        if (c=='!' && src[posi+1]=='='){ add_tok(T_NEQ,"!=",0); posi+=2; coli+=2; continue; }
        if (c=='<' && src[posi+1]=='='){ add_tok(T_LE,"<=",0); posi+=2; coli+=2; continue; }
        if (c=='>' && src[posi+1]=='='){ add_tok(T_GE,">=",0); posi+=2; coli+=2; continue; }
        if (c=='&' && src[posi+1]=='&'){ add_tok(T_AND,"&&",0); posi+=2; coli+=2; continue; }
        if (c=='|' && src[posi+1]=='|'){ add_tok(T_OR,"||",0); posi+=2; coli+=2; continue; }

        // 1-char
        switch(c){
            case '+': add_tok(T_PLUS,"+",0); posi++; coli++; break;
            case '-': add_tok(T_MINUS,"-",0); posi++; coli++; break;
            case '*': add_tok(T_STAR,"*",0); posi++; coli++; break;
            case '/': add_tok(T_SLASH,"/",0); posi++; coli++; break;
            case '(': add_tok(T_LPAREN,"(",0); posi++; coli++; break;
            case ')': add_tok(T_RPAREN,")",0); posi++; coli++; break;
            case '{': add_tok(T_LBRACE,"{",0); posi++; coli++; break;
            case '}': add_tok(T_RBRACE,"}",0); posi++; coli++; break;
            case '=': add_tok(T_EQ,"=",0); posi++; coli++; break;
            case ';': add_tok(T_SEMI,";",0); posi++; coli++; break;
            case ',': add_tok(T_COMMA,",",0); posi++; coli++; break;
            case '<': add_tok(T_LT,"<",0); posi++; coli++; break;
            case '>': add_tok(T_GT,">",0); posi++; coli++; break;
            default: {
                char m[64]; snprintf(m,sizeof m,"invalid char '%c'", c); lex_error(m);
            }
        }
    }
}

// ---------- PARSER BASE ----------
typedef struct { size_t i; } Parser;
static Token* peek(Parser* p){ return &toks[p->i]; }
static Token* get (Parser* p){ return &toks[p->i++]; }
static int accept(Parser* p, TokKind k){ if (peek(p)->kind==k){ p->i++; return 1; } return 0; }
static void expect(Parser* p, TokKind k, const char* msg){
    if (!accept(p,k)){
        Token *t=peek(p);
        fprintf(stderr, "Parse error (%d:%d): expected %s, got kind=%d\n", t->line, t->col, msg, t->kind);
        exit(1);
    }
}

// ---------- STRING BUFFER ----------
typedef struct { char *buf; size_t cap,len; } Str;
static void s_init(Str* s){ s->cap=8192; s->len=0; s->buf=(char*)malloc(s->cap); s->buf[0]='\0'; }
static void s_puts(Str* s, const char* txt){
    size_t n=strlen(txt);
    if (s->len + n + 1 > s->cap){
        while(s->len + n + 1 > s->cap) s->cap *= 2;
        s->buf = (char*)realloc(s->buf, s->cap);
    }
    memcpy(s->buf + s->len, txt, n+1);
    s->len += n;
}
static void s_printf(Str* s, const char* fmt, ...){
    va_list ap; va_start(ap, fmt);
    char tmp[4096];
    int n = vsnprintf(tmp, sizeof tmp, ap);
    va_end(ap);
    if (n<0) return;
    if ((size_t)n >= sizeof tmp){ tmp[sizeof tmp - 2]='\n'; tmp[sizeof tmp - 1]='\0'; n=(int)strlen(tmp); }
    s_puts(s, tmp);
}

// ---------- GLOBAIS ----------
static char gvars[MAX_VARS][MAX_NAME];
static int  ngvars=0;

static int find_gvar(const char* name){ for(int i=0;i<ngvars;i++) if(!strcmp(gvars[i],name)) return i; return -1; }
static void ensure_gvar(const char* name){
    if (find_gvar(name)>=0) return;
    if (ngvars>=MAX_VARS){ fprintf(stderr,"too many globals\n"); exit(1); }
    strncpy(gvars[ngvars], name, MAX_NAME-1); gvars[ngvars][MAX_NAME-1]='\0'; ngvars++;
}

// ---------- CONTEXTO DE FUNÇÃO ----------
typedef struct { char name[MAX_NAME]; int offset; } Local;
static Local locals[MAX_VARS];
static int   nlocals=0;
static int   local_stack=0; // bytes usados (múltiplos de 8)
static int   in_function=0;
static int   ret_label=0;
static char  cur_fn[MAX_NAME];

static int find_local(const char* name){ for(int i=0;i<nlocals;i++) if(!strcmp(locals[i].name,name)) return i; return -1; }
static int ensure_local(const char* name){
    int i = find_local(name);
    if (i>=0) return locals[i].offset;
    if (nlocals>=MAX_VARS){ fprintf(stderr,"too many locals\n"); exit(1); }
    local_stack += 8;
    strncpy(locals[nlocals].name, name, MAX_NAME-1); locals[nlocals].name[MAX_NAME-1]='\0';
    locals[nlocals].offset = local_stack;
    nlocals++;
    return local_stack;
}
static void reset_function_ctx(void){
    nlocals=0; local_stack=0; ret_label=0; cur_fn[0]='\0';
}

// ---------- LABELS ----------
static int next_label_id(void){ static int id=1; return id++; }

// ---------- EXPRS (precedência) ----------
// expr := or
// or := and ( "||" and )*
// and := equality ( "&&" equality )*
// equality := relational ( (==|!=) relational )*
// relational := add ( (<|<=|>|>=) add )*
// add := term ( (+|-) term )*
// term := unary ( (*|/) unary )*
// unary := (- unary) | factor
// factor := NUM | NAME | NAME "(" args? ")" | "(" expr ")"

static void gen_expr_push(Parser* p, Str* body); // fwd

static void gen_var_load_push(Str* body, const char* name){
    if (in_function){
        int idx = find_local(name);
        if (idx>=0){
            int off = locals[idx].offset;
            s_printf(body, "    mov rax, qword ptr [rbp - %d]\n", off);
            s_puts(body,   "    push rax\n");
            return;
        }
    }
    // global
    ensure_gvar(name);
    s_printf(body, "    mov rax, qword ptr [rip + %s]\n", name);
    s_puts(body,   "    push rax\n");
}

static void gen_var_store_from_stack(Str* body, const char* name){
    if (in_function){
        int idx = find_local(name);
        if (idx>=0){
            int off = locals[idx].offset;
            s_puts(body,   "    pop rax\n");
            s_printf(body, "    mov qword ptr [rbp - %d], rax\n", off);
            return;
        }
    }
    ensure_gvar(name);
    s_puts(body,   "    pop rax\n");
    s_printf(body, "    mov qword ptr [rip + %s], rax\n", name);
}

static void gen_call_push(Parser* p, Str* body, const char* fname){
    // args: parse zero or more expressions separated by commas, until ')'
    const char* regs[6] = {"rdi","rsi","rdx","rcx","r8","r9"};
    int nargs = 0;

    if (!accept(p, T_RPAREN)){
        // at least one arg
        do{
            gen_expr_push(p, body); // leaves value on stack
            nargs++;
            if (nargs>6){
                fprintf(stderr, "Error: functions support up to 6 args\n");
                exit(1);
            }
        } while (accept(p, T_COMMA));
        expect(p, T_RPAREN, "')'");
    }

    // pop into regs in reverse to keep arg0 in rdi
    for(int i=nargs-1;i>=0;i--){
        s_printf(body, "    pop %s\n", regs[i]);
    }
    // SysV varargs uses rax = #xmm regs used; p/ não-varargs também é seguro zerar
    s_puts(body, "    xor eax, eax\n");
    s_printf(body, "    call %s\n", fname);
    // retorno já está em rax -> empilha como valor da expressão
    s_puts(body, "    push rax\n");
}

static void gen_factor_push(Parser* p, Str* body){
    Token *t=peek(p);
    if (t->kind==T_NUM){
        long v=t->ival; get(p);
        s_printf(body, "    mov rax, %ld\n", v);
        s_puts(body,   "    push rax\n");
    } else if (t->kind==T_NAME){
        char name[MAX_NAME]; strncpy(name,t->lex,MAX_NAME); get(p);
        if (accept(p, T_LPAREN)){
            // call
            gen_call_push(p, body, name);
        } else {
            gen_var_load_push(body, name);
        }
    } else if (accept(p, T_LPAREN)){
        gen_expr_push(p, body);
        expect(p, T_RPAREN, "')'");
    } else {
        fprintf(stderr,"Parse error (%d:%d): bad factor\n", t->line, t->col); exit(1);
    }
}

static void gen_unary_push(Parser* p, Str* body){
    if (accept(p, T_MINUS)){
        gen_unary_push(p, body);
        s_puts(body,
            "    pop rax\n"
            "    neg rax\n"
            "    push rax\n");
    } else {
        gen_factor_push(p, body);
    }
}

static void gen_term_push(Parser* p, Str* body){
    gen_unary_push(p, body);
    for(;;){
        if (accept(p, T_STAR)){
            gen_unary_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    imul rax, rbx\n"
                "    push rax\n");
        } else if (accept(p, T_SLASH)){
            gen_unary_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    cqo\n"
                "    idiv rbx\n"
                "    push rax\n");
        } else break;
    }
}

static void gen_add_push(Parser* p, Str* body){
    gen_term_push(p, body);
    for(;;){
        if (accept(p, T_PLUS)){
            gen_term_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    add rax, rbx\n"
                "    push rax\n");
        } else if (accept(p, T_MINUS)){
            gen_term_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    sub rax, rbx\n"
                "    push rax\n");
        } else break;
    }
}

static void gen_relational_push(Parser* p, Str* body){
    gen_add_push(p, body);
    for(;;){
        if (accept(p, T_LT)){
            gen_add_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    cmp rax, rbx\n"
                "    setl al\n"
                "    movzx rax, al\n"
                "    push rax\n");
        } else if (accept(p, T_LE)){
            gen_add_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    cmp rax, rbx\n"
                "    setle al\n"
                "    movzx rax, al\n"
                "    push rax\n");
        } else if (accept(p, T_GT)){
            gen_add_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    cmp rax, rbx\n"
                "    setg al\n"
                "    movzx rax, al\n"
                "    push rax\n");
        } else if (accept(p, T_GE)){
            gen_add_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    cmp rax, rbx\n"
                "    setge al\n"
                "    movzx rax, al\n"
                "    push rax\n");
        } else break;
    }
}

static void gen_equality_push(Parser* p, Str* body){
    gen_relational_push(p, body);
    for(;;){
        if (accept(p, T_EQEQ)){
            gen_relational_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    cmp rax, rbx\n"
                "    sete al\n"
                "    movzx rax, al\n"
                "    push rax\n");
        } else if (accept(p, T_NEQ)){
            gen_relational_push(p, body);
            s_puts(body,
                "    pop rbx\n"
                "    pop rax\n"
                "    cmp rax, rbx\n"
                "    setne al\n"
                "    movzx rax, al\n"
                "    push rax\n");
        } else break;
    }
}

static void gen_and_push(Parser* p, Str* body){
    gen_equality_push(p, body);
    while (accept(p, T_AND)){
        int L_false = next_label_id();
        int L_end   = next_label_id();
        // left on stack
        s_puts(body,
            "    pop rax\n"
            "    cmp rax, 0\n");
        s_printf(body, "    je .L%d\n", L_false);
        // right
        gen_equality_push(p, body);
        s_puts(body,
            "    pop rax\n"
            "    cmp rax, 0\n");
        s_printf(body, "    je .L%d\n", L_false);
        s_puts(body, "    mov rax, 1\n");
        s_printf(body, "    jmp .L%d\n", L_end);
        s_printf(body, ".L%d:\n", L_false);
        s_puts(body, "    xor eax, eax\n");
        s_printf(body, ".L%d:\n", L_end);
        s_puts(body, "    push rax\n");
    }
}

static void gen_or_push(Parser* p, Str* body){
    gen_and_push(p, body);
    while (accept(p, T_OR)){
        int L_true = next_label_id();
        int L_end  = next_label_id();
        // left
        s_puts(body,
            "    pop rax\n"
            "    cmp rax, 0\n");
        s_printf(body, "    jne .L%d\n", L_true);
        // right
        gen_and_push(p, body);
        s_puts(body,
            "    pop rax\n"
            "    cmp rax, 0\n");
        s_printf(body, "    jne .L%d\n", L_true);
        s_puts(body, "    xor eax, eax\n");
        s_printf(body, "    jmp .L%d\n", L_end);
        s_printf(body, ".L%d:\n", L_true);
        s_puts(body, "    mov rax, 1\n");
        s_printf(body, ".L%d:\n", L_end);
        s_puts(body, "    push rax\n");
    }
}

static void gen_expr_push(Parser* p, Str* body){ gen_or_push(p, body); }

// ---------- STATEMENTS ----------
static void gen_stmt(Parser* p, Str* body); // fwd

static void gen_block(Parser* p, Str* body){
    if (accept(p, T_LBRACE)){
        while (peek(p)->kind != T_RBRACE){
            gen_stmt(p, body);
        }
        expect(p, T_RBRACE, "'}'");
    } else {
        gen_stmt(p, body);
    }
}

static void gen_stmt(Parser* p, Str* body){
    if (accept(p, T_SEMI)) return;

    if (accept(p, T_LET)){
        Token *id = peek(p);
        if (id->kind != T_NAME){
            fprintf(stderr,"Parse error (%d:%d): expected name after 'let'\n", id->line, id->col); exit(1);
        }
        char name[MAX_NAME]; strncpy(name,id->lex,MAX_NAME); get(p);
        // define local se estamos numa função; senão global
        if (in_function) (void)ensure_local(name); else ensure_gvar(name);
        expect(p, T_EQ, "'='");
        gen_expr_push(p, body);
        expect(p, T_SEMI, "';'");
        gen_var_store_from_stack(body, name);
        return;
    }

    if (accept(p, T_PRINT)){
        gen_expr_push(p, body);
        expect(p, T_SEMI, "';'");
        s_puts(body,
            "    pop rax\n"
            "    mov rsi, rax\n"
            "    lea rdi, [rip + fmt]\n"
            "    xor eax, eax\n"
            "    call printf\n");
        return;
    }

    if (accept(p, T_IF)){
        expect(p, T_LPAREN, "'('");
        gen_expr_push(p, body);
        expect(p, T_RPAREN, "')'");
        int L_else = next_label_id();
        int L_end  = next_label_id();
        s_puts(body,
            "    pop rax\n"
            "    cmp rax, 0\n");
        s_printf(body, "    je .L%d\n", L_else);
        // then
        gen_block(p, body);
        s_printf(body, "    jmp .L%d\n", L_end);
        // else
        s_printf(body, ".L%d:\n", L_else);
        if (accept(p, T_ELSE)) gen_block(p, body);
        s_printf(body, ".L%d:\n", L_end);
        return;
    }

    if (accept(p, T_WHILE)){
        int L_begin = next_label_id();
        int L_end   = next_label_id();
        s_printf(body, ".L%d:\n", L_begin);
        expect(p, T_LPAREN, "'('");
        gen_expr_push(p, body);
        expect(p, T_RPAREN, "')'");
        s_puts(body,
            "    pop rax\n"
            "    cmp rax, 0\n");
        s_printf(body, "    je .L%d\n", L_end);
        gen_block(p, body);
        s_printf(body, "    jmp .L%d\n", L_begin);
        s_printf(body, ".L%d:\n", L_end);
        return;
    }

    if (accept(p, T_RETURN)){
        if (!in_function){ fprintf(stderr,"return fora de função\n"); exit(1); }
        gen_expr_push(p, body);
        expect(p, T_SEMI, "';'");
        s_puts(body, "    pop rax\n");
        s_printf(body, "    jmp .Lret_%s\n", cur_fn);
        return;
    }

    // expressão solta: expr ;
    {
        // lookahead: permitimos chamadas de função/descarte de valor
        // (se não for expr válida, o gerador acusará erro)
        gen_expr_push(p, body);
        expect(p, T_SEMI, "';'");
        s_puts(body, "    add rsp, 8\n"); // descarta 1 valor
        return;
    }
}

// ---------- FUNÇÕES & PROGRAMA ----------
static void gen_function(Parser* p, Str* out_funcs){
    // já consumimos 'fn'
    Token *id = peek(p);
    if (id->kind != T_NAME){ fprintf(stderr,"Parse error (%d:%d): expected function name\n", id->line,id->col); exit(1); }
    strncpy(cur_fn, id->lex, MAX_NAME); cur_fn[MAX_NAME-1]='\0';
    get(p);
    expect(p, T_LPAREN, "'('");

    // reset contexto
    reset_function_ctx();
    in_function = 1;
    ret_label = next_label_id(); // usaremos nomeado com cur_fn (mais legível)

    // parâmetros: até ')'
    const char* regs[6] = {"rdi","rsi","rdx","rcx","r8","r9"};
    Str param_inits; s_init(&param_inits);
    int nparams=0;

    if (!accept(p, T_RPAREN)){
        do{
            Token *t = peek(p);
            if (t->kind != T_NAME){ fprintf(stderr,"Parse error (%d:%d): expected param name\n", t->line,t->col); exit(1); }
            char pname[MAX_NAME]; strncpy(pname,t->lex,MAX_NAME); get(p);
            if (nparams>=6){ fprintf(stderr,"Max 6 params supported\n"); exit(1); }
            int off = ensure_local(pname); // aloca slot
            s_printf(&param_inits, "    mov qword ptr [rbp - %d], %s\n", off, regs[nparams]);
            nparams++;
        } while (accept(p, T_COMMA));
        expect(p, T_RPAREN, "')'");
    }

    // corpo
    expect(p, T_LBRACE, "'{'");

    // Gerar corpo em buffer separado
    Str body; s_init(&body);
    while (peek(p)->kind != T_RBRACE){
        gen_stmt(p, &body);
    }
    expect(p, T_RBRACE, "'}'");

    // Montar função completa
    s_printf(out_funcs, ".text\n.global %s\n%s:\n", cur_fn, cur_fn);
    s_puts(out_funcs,
        "    push rbp\n"
        "    mov rbp, rsp\n"
    );
    if (local_stack>0) s_printf(out_funcs, "    sub rsp, %d\n", local_stack);
    // param moves
    s_puts(out_funcs, param_inits.buf);
    // corpo
    s_puts(out_funcs, body.buf);
    // retorno default (se cair ao fim): rax=0
    s_printf(out_funcs, ".Lret_%s:\n", cur_fn);
    if (local_stack>0) s_printf(out_funcs, "    add rsp, %d\n", local_stack);
    s_puts(out_funcs,
        "    pop rbp\n"
        "    ret\n"
    );

    free(param_inits.buf);
    free(body.buf);
    reset_function_ctx();
    in_function = 0;
}

static void gen_toplevel(Parser* p, Str* out_funcs, Str* out_main){
    while (peek(p)->kind != T_EOF){
        if (accept(p, T_FN)){
            gen_function(p, out_funcs);
        } else {
            // statements em "main"
            gen_stmt(p, out_main);
        }
    }
}

int main(void){
    static char buf[MAX_SRC];
    size_t n = fread(buf,1,MAX_SRC-1, stdin);
    buf[n]='\0';
    src = buf; posi=0; linei=1; coli=1;

    lex_all();

    Parser p = {0};
    Str text_funcs; s_init(&text_funcs);
    Str main_body; s_init(&main_body);
    gen_toplevel(&p, &text_funcs, &main_body);

    // Emitir assembly final
    Str out; s_init(&out);
    s_puts(&out,
        ".intel_syntax noprefix\n"
        ".extern printf\n"
        ".section .rodata\n"
        "fmt: .asciz \"%ld\\n\"\n"
    );
    // funções primeiro
    s_puts(&out, text_funcs.buf);

    // main
    s_puts(&out,
        ".text\n"
        ".global main\n"
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
    for(int i=0;i<ngvars;i++){
        s_printf(&out, "%s: .zero 8\n", gvars[i]);
    }

    fwrite(out.buf,1,out.len,stdout);
    free(out.buf);
    free(text_funcs.buf);
    free(main_body.buf);
    return 0;
}
