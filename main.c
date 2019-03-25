#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>

#define DEBUG 1

void error(char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(1);
}

#define T_NIL          1 // 0000 0000 0000 0001
#define T_IDENT        2 // 0000 0000 0000 0010
#define T_CLOS         4 // 0000 0000 0000 0100
#define T_DATA         8 // 0000 0000 0000 1000
#define T_CONS        16 // 0000 0000 0001 0000
#define T_PRIM        32 // 0000 0000 0010 0000
#define T_STOP        64 // 0000 0000 0100 0000
#define T_CALL       128 // 0000 0000 1000 0000
#define T_BACK       256 // 0000 0001 0000 0000
#define T_RET        512 // 0000 0010 0000 0000
#define T_MACRO     1024 // 0000 0100 0000 0000
#define T_ARGEND    2048 // 0000 1000 0000 0000
#define T_USRMACRO  4096 // 0001 0000 0000 0000
#define T_UNMARK  0x7FFF // 0111 1111 1111 1111
#define T_MARK    0x8000 // 1000 0000 0000 0000

#define MAX_LEN 100

typedef struct _cell {
  int type;
  union {
    int data;
    char ident[MAX_LEN];
    struct {
      struct _cell *car;
      struct _cell *cdr;
    };
    struct {
      struct _cell *env;
      struct _cell *params;
      struct _cell *body;
    };
    struct {
      char name[MAX_LEN];
      void (*func)(struct _cell **, struct _cell**, struct _cell**, struct _cell**);
    };
  };
} cell;

typedef cell* pointer;
typedef void (*primitive_func)(pointer *, pointer *, pointer *, pointer *);

#define TYPE(p, t) ((p)->type & (t))

#define CAR(p)   ((p)->car) 
#define CDR(p)   ((p)->cdr)
#define CAAR(p)  ((p)->car->car)
#define CADR(p)  ((p)->cdr->car)
#define CDAR(p)  ((p)->car->cdr)
#define CDDR(p)  ((p)->cdr->cdr)
#define CAAAR(p) ((p)->car->car->car)
#define CAADR(p) ((p)->cdr->car->car)
#define CADAR(p) ((p)->car->cdr->car)
#define CADDR(p) ((p)->cdr->cdr->car)
#define CDAAR(p) ((p)->car->car->cdr)
#define CDADR(p) ((p)->cdr->car->cdr)
#define CDDAR(p) ((p)->car->cdr->cdr)
#define CDDDR(p) ((p)->cdr->cdr->cdr)

#define CELL_LIMIT 10000000
#define MEMORY_SIZE (CELL_LIMIT * sizeof(cell))
pointer memory, mp;

pointer alloc_memory()
{
  return mmap(NULL, MEMORY_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

pointer nil = &(cell){ T_NIL  };

pointer c_stop    = &(cell){ T_STOP  };
pointer c_call    = &(cell){ T_CALL };
pointer c_back    = &(cell){ T_BACK };
pointer c_ret     = &(cell){ T_RET  };
pointer c_argend  = &(cell){ T_ARGEND };

pointer mk(cell tmp)
{
  if (CELL_LIMIT == mp - memory + 1)
    error("No memory space is available");
  *(++mp) = tmp;
  return mp;
}

pointer make_data(int data)
{
  return mk((cell){ T_DATA, .data = data });
}

pointer make_cons(pointer car, pointer cdr)
{
  return mk((cell){ T_CONS, .car = car, .cdr = cdr });
}

#define CONS(a, b)           make_cons((a), (b))
#define CONS3(a, b, c)       CONS(a, CONS(b, c))
#define CONS4(a, b, c, d)    CONS(a, CONS3(b, c, d))
#define CONS5(a, b, c, d, e) CONS(a, CONS4(b, c, d, e))

pointer make_ident(char *ident)
{
  pointer p = mk((cell){ T_IDENT });
  strcpy(p->ident, ident);
  return p;
}

pointer make_closure(pointer env, pointer params, pointer body)
{
  return mk((cell){ T_CLOS, .env = env, .params = params, .body = body });
}

pointer make_user_macro(pointer env, pointer params, pointer body)
{
  return mk((cell){ T_USRMACRO, .env = env, .params = params, .body = body });
}

void sc(pointer p);

void slist(pointer p)
{
  sc(CAR(p));
  if (TYPE(CDR(p), T_CONS)) {
    printf(" ");
    slist(CDR(p));
  } else if (!TYPE(CDR(p), T_NIL)) {
    printf(" . ");
    sc(CDR(p));
  }
}

void sc(pointer p)
{
  switch(p->type) {
  case T_NIL:    printf("()");                         break;
  case T_CLOS:   printf("#<closure>"); break;
                 // printf("#<closure: (");
                 // sc(p->params);
                 // printf(", ");
                 // sc(p->body);
                 // printf(")>");
                 break;
  case T_IDENT:  printf("%s", p->ident);               break;
  case T_DATA:   printf("%d", p->data);                break;
  case T_CONS:   printf("("); slist(p); printf(")");   break;
  case T_PRIM:   printf("#<primitive: %s>", p->ident); break;
  case T_STOP:   printf("stop");                       break;
  case T_CALL:   printf("call");                       break;
  case T_BACK:   printf("back");                       break;
  case T_RET:    printf("ret");                        break;
  case T_MACRO:  printf("#<macro: %s>", p->ident);     break;
  case T_ARGEND: printf("argend"); break;
  case T_USRMACRO: printf("#<usrmacro>"); break;
  default:       error("Undefined type: %d", p->type);
  }
}

void print_cell(pointer p)
{
  sc(p);
  printf("\n");
}

char lex_buf[MAX_LEN];

char lex()
{
  char c = getchar();
  if (c == ';') {
    do {
      c = getchar();
    } while (c != EOF && c != '\n');
  }
  return c;
}

char space_lex()
{
  char c = lex();
  return isspace(c)?space_lex():c;
}

void unlex(char c)
{
  ungetc(c, stdin);
}

int issymbol(char c)
{
  return !(c == '(' || c == ')' || c == ';');
}

int isident(char c)
{
  return !isspace(c) && issymbol(c);
}

int read_num(char c)
{
  int n = 0;
  do {
    n = (10*n) + (c-'0');
  } while(isdigit(c = lex()));
  unlex(c);
  return n;
}

char *read_ident(char c)
{
  char *ptr = lex_buf;
  do {
    *(ptr++) = c;
  } while(isident(c = lex()));
  unlex(c);
  *ptr = '\0';
  return lex_buf;
}

pointer parse_cell();

pointer parse_list()
{
  pointer p = parse_cell();
  char c = space_lex();
  if (c == '.') {
    pointer q = parse_cell();
    if ((c = space_lex()) != ')')
      error("Unexpected input: '%c'", c);
    return CONS(p, q);
  } else if (c == ')') {
    return CONS(p, nil);
  } else {
    unlex(c);
    return CONS(p, parse_list());
  }
}

pointer parse_cell()
{
  char c;
  while ((c = space_lex()) != EOF) {
    if (isdigit(c)) {
      return make_data(read_num(c));
    } else if (isident(c)) {
      return make_ident(read_ident(c));
    } else if (c == '(') {
      if ((c = space_lex()) == ')')
        return nil;
      unlex(c);
      return parse_list();
    } else {
      error("Unexpeced input: '%c'", c);
    }
  }
  return NULL;
}

pointer reverse(pointer p)
{
  pointer acc = nil;
  while (!TYPE(p, T_NIL)) {
    acc = CONS(CAR(p), acc);
    p   = CDR(p);
  }
  return acc;
}

pointer append(pointer p, pointer q)
{
  if (TYPE(p, T_NIL)) return q;
  else CONS(CAR(p), append(CDR(p), q));
}

pointer zip(pointer p, pointer q)
{
  // p and q are supposed to have the same length.
  pointer ret = nil;
  while (!TYPE(p, T_NIL)) {
    ret = CONS(CONS(CAR(p), CAR(q)), ret);
    p = CDR(p);
    q = CDR(q);
  }
  return reverse(ret);
}

pointer eval(pointer *env, pointer p);
 
pointer lookup(pointer env, char *ident)
{
  if (TYPE(env, T_NIL))
    return make_ident("Error: unbound variable");
  else {
    bool eq = strcmp(CAAR(env)->ident, ident) == 0;
    return eq?CDAR(env):lookup(CDR(env), ident);
  }
}

void add_primitive(pointer *env, int type, char *ident, primitive_func func)
{
  pointer p    = mk((cell){ type, .func = func });
  pointer bind = CONS(make_ident(ident), p);
  strcpy(p->ident, ident);
  *env = CONS(bind, *env);
} 

void macro_lambda(pointer *stk,  pointer *env, pointer *cnt, pointer *dmp)
{
  pointer params = CAAR(*stk);
  pointer body   = CADAR(*stk);
  pointer cls = make_closure(*env, params, body);
  *stk = CONS(cls, CDR(*stk));
}

void macro_macro(pointer *stk,  pointer *env, pointer *cnt, pointer *dmp)
{
  pointer params = CAAR(*stk);
  pointer body   = CADAR(*stk);
  pointer cls = make_user_macro(*env, params, body);
  *stk = CONS(cls, CDR(*stk));
}

void prim_eval(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  printf("expand: "); print_cell(arg);
  *stk = CDR(*stk);
  *cnt = CONS(arg, *cnt);
}

void prim_print(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer ret = nil; 
  for (pointer args = CAR(*stk); args != nil; args = CDR(args))
    print_cell(ret = CAR(args));
  *stk = CONS(ret, CDR(*stk));
}

void macro_quote(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  *stk = CONS(arg, CDR(*stk));
} 

void macro_define(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args = CAR(*stk);
  // code: (_define (quote #ident#) #value#)
  pointer code = CONS3(make_ident("_define"), CONS3(make_ident("quote"), CAR(args), nil), CDR(args));
  *stk = CDR(*stk);
  *cnt = CONS(code, *cnt);
}

void prim_define(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer ident = CAAR(*stk);
  pointer value = CADAR(*stk);
  pointer bind  = CONS(ident, value);
  *stk = CONS(value, CDR(*stk));
  *env = CONS(bind, *env);
}

void prim_cons(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args = CAR(*stk);
  *stk = CONS(CONS(CAR(args), CADR(args)), CDR(*stk));
}

void prim_car(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  *stk = CONS(CAR(arg), CDR(*stk));
}

void prim_cdr(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  *stk = CONS(CDR(arg), CDR(*stk));
}

void prim_list(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  // Do nothing, since the arguments are already pushed to "*stk".
}

void prim_gensym(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  static int id = 0;
  char ident[MAX_LEN];
  sprintf(ident, "_G%03d", id++);
  *stk = CONS(make_ident(ident), CDR(*stk));
  print_cell(*stk);
}

void macro_set(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args = CAR(*stk);
  // code: (_set! (quote #ident#) #value#)
  pointer code = CONS3(make_ident("_set!"), CONS3(make_ident("quote"), CAR(args), nil), CDR(args));
  *stk = CDR(*stk);
  *cnt = CONS(code, *cnt);
}

pointer set(pointer *env, char *ident, pointer value)
{
  if (TYPE(*env, T_NIL))
    return make_ident("Error: unbound variable");
  else {
    bool eq = strcmp(CAAR(*env)->ident, ident) == 0;
    return eq?(CDAR(*env) = value):set(&CDR(*env), ident, value);
  }
}

void prim_set(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args  = CAR(*stk);  
  pointer ident = CAR(args);
  pointer value = CADR(args);
  pointer ret   = set(env, ident->ident, value);
  *stk = CONS(ret, CDR(*stk));
}

void prim_begin(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer p = CAR(*stk);
  if (!TYPE(p, T_NIL)) {
    while (!TYPE(CDR(p), T_NIL))
      p = CDR(p);
  }
  *stk = CONS(CAR(p), CDR(*stk));
}

void macro_delay(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  // cls: The closure of (lambda () #arg#)
  pointer cls = make_closure(*env, nil, arg);
  *stk = CONS(cls, CDR(*stk));
}

void macro_if(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args       = CAR(*stk);
  pointer cond_cell  = CAR(args);
  pointer true_cell  = CADR(args);
  pointer false_cell = CADDR(args);
  pointer dt = CONS3(make_ident("delay"), true_cell, nil);
  pointer df = CONS3(make_ident("delay"), false_cell, nil);
  // code: (force (_if #c# (delay #t#) (delay #f#)))
  pointer code = CONS3(make_ident("force"),
                       CONS5(make_ident("_if"), cond_cell, dt, df, nil),
                       nil);
  *stk = CDR(*stk);
  *cnt = CONS(code, *cnt);
}

void prim_if(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args       = CAR(*stk);
  pointer cond_cell  = CAR(args);
  pointer true_cell  = CADR(args);
  pointer false_cell = CADDR(args);
  assert(TYPE(cond_cell, T_DATA));
  *stk = CONS(cond_cell->data?true_cell:false_cell, CDR(*stk));
}

void prim_bin(pointer *stk, pointer *env, pointer *cnt, pointer *dmp, int (*op)(int, int))
{
  pointer args = CAR(*stk);
  pointer n1   = CAR(args);
  pointer n2   = CADR(args);
  assert(TYPE(n1, T_DATA) && TYPE(n2, T_DATA));
  *stk = CONS(make_data(op(n1->data, n2->data)), CDR(*stk));
}

int bin_add(int a, int b) { return a + b;  }
int bin_sub(int a, int b) { return a - b;  }
int bin_mul(int a, int b) { return a * b;  }
int bin_div(int a, int b) { return a / b;  }
int bin_mod(int a, int b) { return a % b;  }
int bin_eq(int a, int b)  { return a == b; }
int bin_lt(int a, int b)  { return a < b;  }
void prim_add(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  prim_bin(stk, env, cnt, dmp, bin_add);
}
void prim_sub(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  prim_bin(stk, env, cnt, dmp, bin_sub);
}
void prim_mul(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  prim_bin(stk, env, cnt, dmp, bin_mul);
}
void prim_div(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  prim_bin(stk, env, cnt, dmp, bin_div);
}
void prim_mod(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  prim_bin(stk, env, cnt, dmp, bin_mod);
}
void prim_arith_eq(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  prim_bin(stk, env, cnt, dmp, bin_eq);
}
void prim_lt(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  prim_bin(stk, env, cnt, dmp, bin_lt);
}

void prim_eq(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args = CAR(*stk);
  pointer a1 = CAR(args);
  pointer a2 = CADR(args);
  *stk = CONS(make_data(a1==a2?1:0), CDR(*stk));
}

pointer init_env()
{
  pointer env = nil;
  add_primitive(&env, T_MACRO, "lambda", macro_lambda);
  add_primitive(&env, T_MACRO, "macro", macro_macro);
  add_primitive(&env, T_PRIM, "eval", prim_eval);
  add_primitive(&env, T_PRIM, "print", prim_print);
  add_primitive(&env, T_MACRO, "quote", macro_quote);
  // add_primitive(&env, T_MACRO, "define", macro_define);
  add_primitive(&env, T_PRIM, "_define", prim_define);
  add_primitive(&env, T_PRIM, "cons", prim_cons);
  add_primitive(&env, T_PRIM, "car", prim_car);
  add_primitive(&env, T_PRIM, "cdr", prim_cdr);
  add_primitive(&env, T_PRIM, "list", prim_list);
  add_primitive(&env, T_PRIM, "gensym", prim_gensym);
  // add_primitive(&env, T_MACRO, "set!", macro_set);
  add_primitive(&env, T_PRIM, "_set!", prim_set);
  add_primitive(&env, T_PRIM, "begin", prim_begin);
  // add_primitive(&env, T_MACRO, "delay", macro_delay);
  // add_primitive(&env, T_MACRO, "if", macro_if);
  add_primitive(&env, T_PRIM, "_if", prim_if);
  add_primitive(&env, T_PRIM, "+", prim_add);
  add_primitive(&env, T_PRIM, "-", prim_sub);
  add_primitive(&env, T_PRIM, "*", prim_mul);
  add_primitive(&env, T_PRIM, "/", prim_div);
  add_primitive(&env, T_PRIM, "%", prim_mod);
  add_primitive(&env, T_PRIM, "=", prim_arith_eq);
  add_primitive(&env, T_PRIM, "<", prim_lt);
  add_primitive(&env, T_PRIM, "eq?", prim_eq);
  return env;
}

pointer insert_apps(pointer args, pointer last)
{
  if (TYPE(args, T_NIL))
    return last;
  else
    return CONS3(CAR(args), c_call, insert_apps(CDR(args), last));
}

void debug_output(pointer stk, pointer env, pointer cnt, pointer dmp)
{
#if DEBUG
  printf("stk: "); print_cell(stk);
  printf("cnt: "); print_cell(cnt);
  printf("dmp: "); print_cell(dmp);
  printf("env: "); print_cell(env);
  puts("----");
#endif
}

pointer take_args(pointer *stk)
{
  pointer args = nil;
  for (;;) {
    pointer p = CAR(*stk);
    *stk = CDR(*stk);
    if (TYPE(p, T_ARGEND)) break;
    args = CONS(p, args);
  }
  return args;
} 

void run()
{
  pointer stk, env, cnt, dmp;
  pointer p, eval_cell;

  memory = mp = alloc_memory();
  env = init_env();
  eval_cell = lookup(env, "eval");
  while ((p = parse_cell()) != NULL) {
    stk = nil;
    cnt = CONS3(p, c_stop, nil);
    dmp = nil;
    debug_output(stk, env, cnt, dmp);
    while (!TYPE((p = CAR(cnt)), T_STOP)) {
      if (TYPE(p, T_NIL | T_DATA | T_CLOS)) {
        stk = CONS(p, stk);
        cnt = CDR(cnt);
      } else if (TYPE(p, T_IDENT)) {
        stk = CONS(lookup(env, p->ident), stk);
        cnt = CDR(cnt);
      } else if (TYPE(p, T_BACK)) {
        pointer func     = CAR(stk);
        pointer args     = CAAR(dmp);
        pointer tail_cnt = CDAR(dmp);
        assert(TYPE(func, T_CLOS | T_MACRO | T_PRIM | T_USRMACRO));
        if (TYPE(func, T_PRIM | T_CLOS)) {
          stk = CONS(c_argend, stk);
          cnt = append(args, CONS(c_call, tail_cnt));
        } else if (TYPE(func, T_MACRO | T_USRMACRO)) {
          stk = append(reverse(args), CONS(c_argend, stk));
          cnt = CONS(c_call, tail_cnt);
        }
        dmp = CDR(dmp);
      } else if (TYPE(p, T_CONS)) {
        dmp = CONS(CONS(CDR(p), CDR(cnt)), dmp);
        cnt = CONS3(CAR(p), c_back, nil);
      } else if (TYPE(p, T_CALL)) {
        pointer args = take_args(&stk);
        pointer func = CAR(stk);
        stk = CDR(stk);
        cnt = CDR(cnt);
        if (TYPE(func, T_PRIM | T_MACRO)) {
          stk = CONS(args, stk);
          func->func(&stk, &env, &cnt, &dmp);
        } else if (TYPE(func, T_CLOS)) {
          // TODO: implement partial evaluation (when |func->params| > |args|)
          pointer binds   = zip(func->params, args);
          pointer cls_env = append(binds, func->env);
          pointer body    = CONS3(func->body, c_ret, nil);
          dmp = CONS4(stk, env, cnt, dmp);
          stk = nil;
          env = cls_env;
          cnt = body;
        } else if (TYPE(func, T_USRMACRO)) {
          // TODO: implement partial evaluation (when |func->params| > |args|)
          pointer binds   = zip(func->params, args);
          pointer cls_env = append(binds, func->env);
          pointer body    = CONS3(func->body, c_ret, nil);
          stk = CONS3(c_argend, eval_cell, stk);
          cnt = CONS(c_call, cnt);
          dmp = CONS4(stk, env, cnt, dmp);
          stk = nil;
          env = cls_env;
          cnt = body;
        } else {
          error("Invalid function call");
        }
      } else if (TYPE(p, T_RET)) {
        stk = CONS(CAR(stk), CAR(dmp));
        env = CADR(dmp);
        cnt = CADDR(dmp);
        dmp = CDDDR(dmp);
      } else {
        error("Undefined execution: %d", p->type);
      }
      debug_output(stk, env, cnt, dmp);
    }
    assert(TYPE(dmp, T_NIL));
  }
}

int main(int argc, char **argv)
{
  run();
  return 0;
}
