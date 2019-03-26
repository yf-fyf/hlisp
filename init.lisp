; -- List of primitive functions -- 
; lambda
; macro
; eval
; print
; quote
; _define
; _add-reader-macro
; cons
; car
; cdr
; gensym
; _set!
; begin
; _if
; +
; -
; *
; /
; %
; =
; <
; eq?
; pair?
; ----

; -- About "_define" --
; The primitive "_define" evaluates given arguments strictly, 
; so that we have to wrap the identifier-part by "quote" to invoke it.
; For instance, "(_define x 123)" does not work since "x" is turned out to be unbound,
; and hence we need to write "(_define (quote x) 123)" to record the value to "x".
; ----

; -- About "macro" --
; Syntax: (macro (x1 x2 ... xn) body)
; (Variadic representations (macro args body) and (macro (x1 ... xn . args) body) are also available)
; 
; Macro is one of first-class citizens in this calculus, and is designed analogously to
; the closure of lambda abstraction (i.e., (lambda (x1 x2 ... xn) body)).
;
; The intuitive execution of the applicaiton ((macro (x1 x2 ... xn) body) e1 e2 ... en) is summarized as follows:
; (1) First, the arguments e1, ..., en are passed to "body" with "quote" wrappings:
;
;     ((macro (x1 x2 ... xn) body) e1 e2 ... en)
;     ~> body[x1 := (quote e1), x2 := (quote e2), ..., xn := (quote en)]
;     ~> ...
;     ~> body' ; The result of the computation of "body[x1 := (quote e1), x2 := (quote e2), ..., xn := (quote en)]"
;
;     Note that the original arguments e1, ..., en are *not* evaluated in this stage.
;
; (2) Then, the final value is obtaines as the result of application "eval" to "body'", i.e., as the following:
;
;     (eval body')
;     ~> ...
;     ~> value ; The final result
; 
; -- Example --
; Macro-style K combinator, (macro (x y) x)
;
; Computation example: ((macro (x y) x) (print 42) (print 0))
; ((macro (x y) x) (print 42) (print 0))
; ~> (eval (quote (print 42)))
; ~> (eval '(print 42))
; ~> (print 42) ~> ... ~> 42 (with the side effect of printing "42")
; where the value of "(quote e)" is represented as "'e".
;
; Note that the argument "(print 0)" is not evaluated (there is no side effect by "(print 0)")
;
; -- Example --
; A macro of user-defined "define", which will be used as "defvar" in the remaining program codes,
; (macro (id e) (list (quote _define) (list (quote quote) id) e))
; 
; Computation example: ((macro (id e) (list (quote _define) (list (quote quote) id) e)) x 123)
; ((macro (id e) (list (quote _define) (list (quote quote) id) e)) x 123)
; ~> (eval (list (quote _define) (list (quote quote) (quote x)) (quote 123)))
; ~> ...
; ~> (eval '(_define (quote x) 123))
; ~> (_define (quote x) 123) ~> ... ~> 123 (with the side effect of recording the value "123" to "x") 
; -----

(_define (quote list)
  ; A variadic function
  (lambda ls ls))

(_define (quote defvar) (macro (id e)
  ; `(_define (quote ,id) ,e)
  (list (quote _define)
        (list (quote quote) id)
        e)))

(defvar define-macro (macro (ls . body)
  ; `(defvar ,(car ls) (macro ,(cdr ls) ,@body))
  (list (quote defvar) (car ls)
        (cons (quote macro) (cons (cdr ls) body)))))

(define-macro (set! id e)
  ; `(_set! (quote ,id) ,e) 
  (list (quote _set!) (list (quote quote) id) e))

(define-macro (defun name params . body)
  ; Any recursive function is defined by the so-called "back-patching" technique.
  ; `(begin (defvar ,name ()) (set! ,name (lambda ,params ,@body)))
  (list (quote begin)
        (list (quote defvar) name ())
        (list (quote set!) name
              (cons (quote lambda) (cons params body)))))

(define-macro (delay x)
  ; `(lambda () ,x)
  (list (quote lambda) () x))

(defun force (f) (f))

(define-macro (if x y z)
  ; `(force (_if ,x (delay ,y) (delay ,z)))
  (list (quote force)
        (list (quote _if) x
              (list (quote delay) y)
              (list (quote delay) z))))

(define-macro (define ls . e)
  ; (if (pair? ls) `(defun ,(car ls) ,(cdr ls) ,@e) `(defvar ,ls ,(car e)))
  (if (pair? ls) (cons (quote defun) (cons (car ls) (cons (cdr ls) e)))
                 (list (quote defvar) ls (car e))))

(_add-reader-macro ' quote)

(define null ())
(define (null? x) (eq? x null))
(define (atom? x) (not (pair? x)))
(define (_ n) (- 0 n))
(define (not x) (if x 0 1))

(define-macro (and x y)
  ; `(if ,x (if ,y 1 0) 0)
  (list 'if x
        (list 'if y 1 0) 0))

(define-macro (or x y)
  ; `(if ,x 1 (if ,y 1 0))
  (list 'if x 1
        (list 'if y 1 0)))

(define (<= x y) (or (< x y) (= x y)))
(define (> x y) (< y x))
(define (>= x y) (<= y x))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (iota n)
  (define (iota0 i)
    (if (> i n) null
        (cons i (iota0 (+ i 1)))))
  (iota0 0))

(define (map f ls)
  (if (null? ls) null
      (cons (f (car ls)) (map f (cdr ls)))))

(define (reverse ls)
  (define (reverse0 ls acc)
      (if (null? ls) acc
          (reverse0 (cdr ls) (cons (car ls) acc))))
  (reverse0 ls null))

(define (filter f ls)
  (define (filter0 ls acc)
      (if (null? ls) (reverse acc)
          (filter0 (cdr ls)
                   (if (f (car ls))
                       (cons (car ls) acc)
                       acc))))
  (filter0 ls null))

(define (print_list ls)
  (if (null? ls) null
      (begin
        (print (car ls))
        (print_list (cdr ls)))))

(define (last ls)
  (if (null? ls) null
      (if (null? (cdr ls)) (car ls)
          (last (cdr ls)))))

