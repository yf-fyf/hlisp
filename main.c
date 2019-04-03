#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
#include <sys/mman.h>

#define DEBUG 0

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
#define T_DATA         4 // 0000 0000 0000 0100
#define T_CONS         8 // 0000 0000 0000 1000
#define T_CLOS        16 // 0000 0000 0001 0000
#define T_USRMACRO    32 // 0000 0000 0010 0000
#define T_PRIM        64 // 0000 0000 0100 0000
#define T_MACRO      128 // 0000 0000 1000 0000
#define T_STOP       256 // 0000 0001 0000 0000
#define T_BACK       512 // 0000 0010 0000 0000
#define T_CALL      1024 // 0000 0100 0000 0000
#define T_ARGEND    2048 // 0000 1000 0000 0000
#define T_RET       4096 // 0001 0000 0000 0000
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
pointer alloc_memory() { return malloc(MEMORY_SIZE); }

// ---- global variables ----
pointer memory, mp;    // momery, and memory pointer
char lex_buf[MAX_LEN]; // temporary variable for lexer
pointer renv;          // environment for reader macro
pointer eval_cell;     // for eval call during evaluation of user macros
// --------

// ---- global constants ----
const pointer nil      = &(cell){ T_NIL  };
const pointer c_stop   = &(cell){ T_STOP  };
const pointer c_call   = &(cell){ T_CALL };
const pointer c_back   = &(cell){ T_BACK };
const pointer c_ret    = &(cell){ T_RET  };
const pointer c_argend = &(cell){ T_ARGEND };
// --------

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

pointer make_closure(int type, pointer env, pointer params, pointer body)
{
  return mk((cell){ type, .env = env, .params = params, .body = body });
}

void pc(pointer p);

void plist(pointer p)
{
  pc(CAR(p));
  if (TYPE(CDR(p), T_CONS)) {
    printf(" ");
    plist(CDR(p));
  } else if (!TYPE(CDR(p), T_NIL)) {
    printf(" . ");
    pc(CDR(p));
  }
}

void pc(pointer p)
{
  switch(p->type) {
  case T_NIL:      printf("()");                         break;
  case T_IDENT:    printf("%s", p->ident);               break;
  case T_DATA:     printf("%d", p->data);                break;
  case T_CONS:     printf("("); plist(p); printf(")");   break;
  case T_CLOS:     printf("#<closure>");                 break;
  case T_USRMACRO: printf("#<usrmacro>");                break;
  case T_PRIM:     printf("#<primitive: %s>", p->ident); break;
  case T_MACRO:    printf("#<macro: %s>", p->ident);     break;
  case T_STOP:     printf("stop");                       break;
  case T_BACK:     printf("back");                       break;
  case T_CALL:     printf("call");                       break;
  case T_ARGEND:   printf("argend");                     break;
  case T_RET:      printf("ret");                        break;
  default:         error("Undefined type: %d", p->type);
  }
}

void print_cell(pointer p)
{
  pc(p); printf("\n");
}

char lex()
{
  char c = getchar();
  if (c == ';') {
    do {
      c = getchar(); // skip a line comment
    } while (c != EOF && c != '\n');
  }
  return c;
}

char space_lex()
{
  char c;
  while (isspace(c = lex()))
    ;
  return c;
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

char *strrev(char *str)
{
  int i, len = strlen(str);
  for (i = 0; i < len/2; i++) {
    char tmp = str[i];
    str[i] = str[len-i-1];
    str[len-i-1] = tmp;
  }
  return str;
}

pointer parse_cell()
{
  char c;
  while ((c = space_lex()) != EOF) {
    if (isdigit(c)) {
      return make_data(read_num(c));
    } else if (isident(c)) {
      char *ident = read_ident(c);
      {// For reader macro
        pointer p, macro, rest;
        char *postfix;
        p = renv;
        while (!TYPE(p, T_NIL)) {
          macro = CAR(p);
          if (strstr(ident, CAR(macro)->ident) != NULL) {
            postfix = strrev(ident+strlen(CAR(macro)->ident));
            while (*postfix != '\0')
              unlex(*(postfix++));
            rest = parse_cell(); 
            return CONS3(CDR(macro), rest, nil);
          }
          p = CDR(p);
        }
      }
      return make_ident(ident);
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
  p = reverse(p);
  while (!TYPE(p, T_NIL)) {
    q = CONS(CAR(p), q);
    p = CDR(p);
  }
  return q;
}

pointer zip(pointer p, pointer q)
{
  // Assumption: |p| <= |q|, where |x| means the length of x.
  pointer ret = nil;
  for (;;) {
    assert(TYPE(p, T_NIL | T_IDENT | T_CONS));
    if (TYPE(p, T_NIL)) {
      break;
    } else if (TYPE(p, T_IDENT)) {// If |p| < |q|. (Binds a variadic parameter)
      ret = CONS(CONS(p, q), ret);
      break;
    } else if (TYPE(p, T_CONS)) {
      ret = CONS(CONS(CAR(p), CAR(q)), ret);
      p = CDR(p);
      q = CDR(q);
    }
  }
  return reverse(ret);
}
 
pointer lookup(pointer env, char *ident)
{
  while (!TYPE(env, T_NIL)) {
    if (strcmp(CAAR(env)->ident, ident) == 0)
      return CDAR(env);
    else
      env = CDR(env);
  }
  error("Unbound variable: \"%s\"", ident);
}

void add_primitive(pointer *env, int type, char *ident, primitive_func func)
{
  pointer p    = mk((cell){ type, .func = func });
  pointer bind = CONS(make_ident(ident), p);
  strcpy(p->ident, ident);
  *env = CONS(bind, *env);
} 

void macro_closure(int type, pointer *stk,  pointer *env, pointer *cnt, pointer *dmp)
{
  pointer params = CAAR(*stk);
  pointer body   = CONS(make_ident("begin"), CDAR(*stk));
  pointer cls    = make_closure(type, *env, params, body);
  *stk = CONS(cls, CDR(*stk));
}

void macro_lambda(pointer *stk,  pointer *env, pointer *cnt, pointer *dmp)
{
  macro_closure(T_CLOS, stk, env, cnt, dmp);
}

void macro_macro(pointer *stk,  pointer *env, pointer *cnt, pointer *dmp)
{
  macro_closure(T_USRMACRO, stk, env, cnt, dmp);
}

void def_reader_macro(pointer ident, pointer value)
{
  pointer bind = CONS(ident, value);
  renv = CONS(bind, renv);
}

void prim_eval(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  *stk = CDR(*stk);
  *cnt = CONS(arg, *cnt);
  // printf("Eval: "); print_cell(arg);
}

void prim_print(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args, ret;
  for (args = CAR(*stk), ret = nil; args != nil; args = CDR(args))
    print_cell(ret = CAR(args));
  *stk = CONS(ret, CDR(*stk));
}

void macro_quote(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  *stk = CONS(arg, CDR(*stk));
} 

void prim_define(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer ident = CAAR(*stk);
  pointer value = CADAR(*stk);
  pointer bind  = CONS(ident, value);
  *stk = CONS(value, CDR(*stk));
  *env = CONS(bind, *env);
}

void prim_def_reader_macro(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args = CAR(*stk);
  def_reader_macro(CAR(args), CADR(args));
  *stk = CDR(*stk);
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

void prim_gensym(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  static int id = 0;
  char ident[MAX_LEN];
  sprintf(ident, "_G%03d", id++);
  *stk = CONS(make_ident(ident), CDR(*stk));
}

pointer set(pointer env, char *ident, pointer value)
{
  pointer p = env;
  while (!TYPE(p, T_NIL)) {
    if (strcmp(CAAR(p)->ident, ident) == 0)
      return (CDAR(p) = value);
    p = CDR(p);
  }
  error("Unbound variable: \"%s\"", ident);
}

void prim_set(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args  = CAR(*stk);  
  pointer ident = CAR(args);
  pointer value = CADR(args);
  pointer ret   = set(*env, ident->ident, value);
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

#define DEFINE_ARITH_PRIMITIVE(name, op)                         \
int bin_##name(int x, int y) { return x op y; }                  \
void prim_##name(pointer *s, pointer *e, pointer *c, pointer *d) \
{                                                                \
  return prim_bin(s, e, c, d, bin_##name);                       \
}
DEFINE_ARITH_PRIMITIVE(add, +)
DEFINE_ARITH_PRIMITIVE(sub, -)
DEFINE_ARITH_PRIMITIVE(mul, *)
DEFINE_ARITH_PRIMITIVE(div, /)
DEFINE_ARITH_PRIMITIVE(mod, %)
DEFINE_ARITH_PRIMITIVE(eq, ==)
DEFINE_ARITH_PRIMITIVE(lt,  <)

void prim_eqp(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args = CAR(*stk);
  pointer a1 = CAR(args);
  pointer a2 = CADR(args);
  bool eq;
  if (TYPE(a1, T_IDENT) && TYPE(a2, T_IDENT))
    eq = strcmp(a1->ident, a2->ident) == 0;
  else if (TYPE(a1, T_DATA) && TYPE(a2, T_DATA))
    eq = a1->data == a2->data;
  else
    eq = a1 == a2;
  *stk = CONS(make_data(eq?1:0), CDR(*stk));
}

void prim_pairp(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer arg = CAAR(*stk);
  *stk = CONS(make_data(TYPE(arg, T_CONS)?1:0), CDR(*stk));
}

pointer init_env()
{
  pointer env = nil;
  add_primitive(&env, T_MACRO, "lambda", macro_lambda);
  add_primitive(&env, T_MACRO, "macro", macro_macro);
  add_primitive(&env, T_PRIM, "eval", prim_eval);
  add_primitive(&env, T_PRIM, "print", prim_print);
  add_primitive(&env, T_MACRO, "quote", macro_quote);
  add_primitive(&env, T_PRIM, "_define", prim_define);
  add_primitive(&env, T_PRIM, "_def-reader-macro", prim_def_reader_macro);
  add_primitive(&env, T_PRIM, "cons", prim_cons);
  add_primitive(&env, T_PRIM, "car", prim_car);
  add_primitive(&env, T_PRIM, "cdr", prim_cdr);
  add_primitive(&env, T_PRIM, "gensym", prim_gensym);
  add_primitive(&env, T_PRIM, "_set!", prim_set);
  add_primitive(&env, T_PRIM, "begin", prim_begin);
  add_primitive(&env, T_PRIM, "_if", prim_if);
  add_primitive(&env, T_PRIM, "+", prim_add);
  add_primitive(&env, T_PRIM, "-", prim_sub);
  add_primitive(&env, T_PRIM, "*", prim_mul);
  add_primitive(&env, T_PRIM, "/", prim_div);
  add_primitive(&env, T_PRIM, "%", prim_mod);
  add_primitive(&env, T_PRIM, "=", prim_eq);
  add_primitive(&env, T_PRIM, "<", prim_lt);
  add_primitive(&env, T_PRIM, "eq?", prim_eqp);
  add_primitive(&env, T_PRIM, "pair?", prim_pairp);
  return env;
}

//============================================
// Transition rule of Exntended SECD Machine
//============================================
//  Case: T_STOP
//    < S, E, stop, D > =/=>
// 
//    Remark: This means that whole evaluation of a s-exp is completed.
//            The final value is in the top of S.
// 
//  Case: Value (T_NIL | T_DATA | T_CLOS | T_USRMACRO)
//    < S, E, (v . C), D >
//    => < (v . S), E, C, D >
// 
//  Case: T_CONS
//    < S, E, ((e1 e2 ... en) . C), D >
//    => < S, E, (e1 c_back), (((e2 ... en) . C) . D)>
//    Remark: We focus on the computation of the head (i.e., e1),
//            because the remaining computation depends on two cases:
//            (1) e2 ... en will be evaluated if e1 reduces to a function;
//            (2) e2 ... en will not be evaluated if e1 reduces to a macro.
// 
//  Case: T_BACK
//    Subcase: When the top of stack, v1, is #<primitive> or #<closure>.
//    < (v1 . S), E, (back . nil), (((e2 ... en) . C). D) >
//    => < (c_argend . S), E, (e2 .. en c_call C), D >   if v1 is #<primitive> or #<closure>
//    Remark: The arguments must be evaluated so that
//            we push e2 .. en to cnt-part.
// 
//    Subcase: When the top of stack, v1, is #<macro> or #<usrmacro>.
//      < (v1 . S), E, (back . nil), (((e2 ... en) . C). D) >
//      => < (en .. e2 c_argend S), E, (c_call . C), D >   if v1 is #<macro> or #<usrmacro>
//    Remark: The arguments must *not* be evaluated so that
//            we push e2 .. en to stk-part immediately.
// 
//  Case: T_CALL
//    Subcase: If u is #<primitive>
//      < (vn ... v1 c_argend u S), E, (c_call . C), D >
//      => < ((v1 ... vn) . S), E, C, D >
//      => ... the execution by #<primitive> ...
//      => < S', E', C', D' >
//      where S', E', C', and D' are the result of the primitive function call.
// 
//    Subcase: If u is #<macro>
//      < (en ... e1 c_argend u S), E, (c_call . C), D >
//      => < ((e1 ... en) . S), E, C, D >
//      => ... the execution by #<macro> ...
//      => < S', E', C', D' >
//      where S', E', C', and D' are the result of the primitive macro call.
// 
//    Subcase: If u is #<closure> (we omit the case of variadic functions)
//      Suppose that "u" is the colosure of (lambda (x1 ... xn) body) 
//      with E' which is its closure environment. Then,
//      < (vn ... v1 c_argend u S), E, (c_call . C), D >
//      => < nil, ( (x1 . v1) (x2 . v2) ... (xn . vn) E' ), (body c_ret), (S E C D)>
// 
//    Subcase: If u is #<usrmacro> (we omit the case of variadic macros)
//      Suppose that "u" is the closure of (macro (x1 ... xn) body) 
//      with E' which is its closure environment. Then,
//      < (en ... e1 c_argend u S), E, (c_call . C), D >
//      => < nil, ( (x1 . e1) (x2 . e2) ... (xn . en) E' ), (body c_ret), ((c_argend #<primitive: eval> S) E (c_call . C) D)>
// 
//  Case: T_RET
//    < (v . S), E, (c_ret . nil), (S' E' C' D')>
//    => < (v . S'), E', C', D' >
//============================================
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

void op_value(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  *stk = CONS(CAR(*cnt), *stk);
  *cnt = CDR(*cnt);
}

void op_ident(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  *stk = CONS(lookup(*env, CAR(*cnt)->ident), *stk);
  *cnt = CDR(*cnt);
}

void op_cons(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  *dmp = CONS(CONS(CDAR(*cnt), CDR(*cnt)), *dmp);
  *cnt = CONS3(CAAR(*cnt), c_back, nil);
}

void op_back(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer func     = CAR(*stk);
  pointer args     = CAAR(*dmp);
  pointer tail_cnt = CDAR(*dmp);
  assert(TYPE(func, T_CLOS | T_MACRO | T_PRIM | T_USRMACRO));
  if (TYPE(func, T_PRIM | T_CLOS)) {
    *stk = CONS(c_argend, *stk);
    *cnt = append(args, CONS(c_call, tail_cnt));
  } else if (TYPE(func, T_MACRO | T_USRMACRO)) {
    *stk = append(reverse(args), CONS(c_argend, *stk));
    *cnt = CONS(c_call, tail_cnt);
  }
  *dmp = CDR(*dmp);
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

void op_call(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  pointer args = take_args(stk);
  pointer func = CAR(*stk);
  *stk = CDR(*stk);
  *cnt = CDR(*cnt);
  assert(TYPE(func, T_PRIM | T_MACRO | T_CLOS | T_USRMACRO)); 
  if (TYPE(func, T_PRIM | T_MACRO)) {
    *stk = CONS(args, *stk);
    func->func(stk, env, cnt, dmp);
  } else if (TYPE(func, T_CLOS | T_USRMACRO)) {
    pointer cls_env, body; 
    if (TYPE(func->params, T_IDENT)) {// If the closure is a variadic function/macro
      pointer bind = CONS(func->params, args);
      cls_env = CONS(bind, func->env);
    } else {
      pointer binds = zip(func->params, args);
      cls_env = append(binds, func->env);
    }
    body = CONS3(func->body, c_ret, nil);
    if (TYPE(func, T_CLOS))
      *dmp = CONS4(*stk, *env, *cnt, *dmp);
    else if (TYPE(func, T_USRMACRO))
      *dmp = CONS4(CONS3(c_argend, eval_cell, *stk), *env, CONS(c_call, *cnt), *dmp);
    *stk = nil;
    *env = cls_env;
    *cnt = body;
  }
}

void op_ret(pointer *stk, pointer *env, pointer *cnt, pointer *dmp)
{
  *stk = CONS(CAR(*stk), CAR(*dmp));
  *env = CADR(*dmp);
  *cnt = CADDR(*dmp);
  *dmp = CDDDR(*dmp);
}

void run()
{
  pointer stk, env, cnt, dmp;
  pointer p;

  memory    = mp = alloc_memory();
  renv      = nil;
  env       = init_env();
  eval_cell = lookup(env, "eval");
  while ((p = parse_cell()) != NULL) {
    stk = nil;
    cnt = CONS3(p, c_stop, nil);
    dmp = nil;
    do {
      debug_output(stk, env, cnt, dmp);
      if (TYPE(p, T_NIL | T_DATA | T_CLOS | T_USRMACRO | T_PRIM | T_MACRO))
        op_value(&stk, &env, &cnt, &dmp);
      else if (TYPE(p, T_IDENT))
        op_ident(&stk, &env, &cnt, &dmp);
      else if (TYPE(p, T_CONS))
        op_cons(&stk, &env, &cnt, &dmp);
      else if (TYPE(p, T_BACK))
        op_back(&stk, &env, &cnt, &dmp);
      else if (TYPE(p, T_CALL))
        op_call(&stk, &env, &cnt, &dmp);
      else if (TYPE(p, T_RET))
        op_ret(&stk, &env, &cnt, &dmp);
      else
        error("Undefined execution: %d", p->type);
    } while (!TYPE((p = CAR(cnt)), T_STOP));
    assert(TYPE(dmp, T_NIL));
  }
  debug_output(stk, env, cnt, dmp);
}

int main(int argc, char **argv)
{
  run();
  return 0;
}
