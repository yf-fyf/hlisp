; -- List of primitive functions -- 
; lambda
; macro
; eval
; print
; quote
; _define
; _def-reader-macro
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
;     => body[x1 := (quote e1), x2 := (quote e2), ..., xn := (quote en)]
;     ~> ...
;     ~> body' ; The result of the computation of "body[x1 := (quote e1), x2 := (quote e2), ..., xn := (quote en)]"
;
;     Note that the original arguments e1, ..., en are *not* evaluated in this stage.
;
; (2) Then, the final value is obained by applying "eval" to "body'" as follows:
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

(define null ())
(define (null? x) (eq? x null))
(define (atom? x) (not (pair? x)))
(define (_ n) (- 0 n))
(define (not x) (if x 0 1))

(define-macro (define-reader-macro id e)
  (list (quote _def-reader-macro) (list (quote quote) id) e))

(define-reader-macro ' quote)

(define-macro (binary-and x y)
  ; `(if ,x (if ,y 1 0) 0)
  (list 'if x
        (list 'if y 1 0) 0))

(define-macro (and . ls)
  (define (rec ls)
    (if (null? ls) 1
        (list 'binary-and (car ls) (rec (cdr ls)))))
  (rec ls))

(define-macro (binary-or x y)
  ; `(if ,x 1 (if ,y 1 0))
  (list 'if x 1
        (list 'if y 1 0)))

(define-macro (or . ls)
  (define (rec ls)
    (if (null? ls) 0
        (list 'binary-or (car ls) (rec (cdr ls)))))
  (rec ls))

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

(define-macro (cond . ls)
  (define (rec ls)
    (if (null? ls) ()
        (list 'if (caar ls)
                  (cadar ls)
                  (rec (cdr ls)))))
  (rec ls))

(define else 1)

(define (iota n)
  (define (rec i)
    (if (= i n) null
        (cons i (rec (+ i 1)))))
  (rec 0))

(define (map f ls)
  (if (null? ls) null
      (cons (f (car ls)) (map f (cdr ls)))))

(define (fold f v ls)
  (define (rec ls acc)
    (if (null? ls) acc
        (rec (cdr ls) (f (car ls) acc))))
  (rec ls v))

(define (fold-right f v ls)
  (define (rec ls)
    (if (null? ls) v
        (f (car ls) (rec (cdr ls)))))
  (rec ls))

(define (reverse ls)
  (define (rec ls acc)
      (if (null? ls) acc
          (rec (cdr ls) (cons (car ls) acc))))
  (rec ls null))

(define (binary-append ls1 ls2)
  (define (rec ls1 acc)
    (if (null? ls1) acc
        (rec (cdr ls1) (cons (car ls1) acc))))
  (rec (reverse ls1) ls2))

(define (append . ls)
  (fold-right binary-append null ls))

(define (filter f ls)
  (define (rec ls acc)
      (if (null? ls) (reverse acc)
          (rec (cdr ls)
                   (if (f (car ls))
                       (cons (car ls) acc)
                       acc))))
  (rec ls null))

(define (remove f ls)
  (filter (lambda (x) (not (f x))) ls))

(define (last ls) (car (reverse ls)))

(define (zip . lss)
  (define (rec lss acc)
    (if (null? (car lss))
        (reverse acc)
        (rec (map cdr lss) (cons (map car lss) acc))))
  (rec lss null))

(define (unzip1 ls) (map car ls))

(define (unzip2 ls)
  (list (map car ls) (map cadr ls)))

(define (unzip3 ls)
  (list (map car ls)
        (map cadr ls)
        (map caddr ls)))

; ==== Quasiquote expansion ====
; Reference: Alan Bawden. Quasiquotation in Lisp. In proceedings of PEPM, pp.4--12, 1999.
;
; The algorithm to implement "quasiquote expansion" relies on 
; the code by Bawden in Appendix B of the above paper.
;
(define qq-expand null)      ; An initialization for mutual recursion.
(define qq-expand-list null) ; Same as above.

(set! qq-expand (lambda (x depth)
  (cond ((not (pair? x)) (list 'quote x))
        ((eq? (car x) 'quasiquote)
         (list 'cons
               (list 'quote 'quasiquote)
               (qq-expand (cdr x) (+ depth 1))))
        ((or (eq? (car x) 'unquote) (eq? (car x) 'unquote-splicing))
                     (cond ((> depth 0)
                            (list 'cons (list 'quote (car x))
                                        (qq-expand (cdr x) (- depth 1))))
                           ((and (eq? 'unquote (car x)) (not (null? (cdr x))) (null? (cddr  x)))
                            (cadr x))
                           (else (print "Illegal"))))
        (else (list 'append  (qq-expand-list (car x) depth)
                             (qq-expand (cdr x) depth))))))

(set! qq-expand-list (lambda (x depth)
  (cond ((not (pair? x)) (list 'quote (list x)))
        ((eq? (car x) 'quasiquote)
         (list 'list (list 'cons (list 'quote 'quasiquote)
                                 (qq-expand (cdr x) (+ depth 1)))))
        ((or (eq? (car x) 'unquote) (eq? (car x) 'unquote-splicing))
         (cond ((> depth 0)
                (list 'list
                      (list 'cons
                            (list 'quote (car x))
                            (qq-expand (cdr x) (- depth 1)))))
               ((eq? 'unquote (car x))
                (cons 'list (cdr x)))
               (else (cons 'append (cdr x)))))
        (else (list 'list
                    (list 'append
                          (qq-expand-list (car x) depth)
                          (qq-expand (cdr x) depth)))))))

(define (_quasiquote-expand x) (qq-expand (cadr x) 0))
(define-macro (quasiquote-expand x)
  (eval (list '_quasiquote-expand (list 'quote x))))
; ========

(define quasiquote (macro (x) (list 'quasiquote-expand (list 'quote x))))
(define-reader-macro ` 'quasiquote)
(define-reader-macro ,@ 'unquote-splicing)
(define-reader-macro , 'unquote)

(define-macro (push expr ident)
  `(set! ,ident (cons ,expr ,ident)))

(define-macro (pop ident)
  (define tmp (gensym))
  `(begin (define ,tmp (car ,ident))
          (set! ,ident (cdr ,ident))
          ,tmp))

(define-macro (unless cond then else)
  `(if (not ,cond) ,then ,else))

(define-macro (let binds . body)
  (define vars (map car binds))
  (define prms (map cadr binds))
  `((lambda ,vars ,@body) ,@prms))

(define-macro (let* binds . body)
  (define vars (map car binds))
  (define prms (map cadr binds))
  (define init (map (lambda (p) `(define ,(car p) ,(cadr p))) binds))
  `((lambda () ,@init ,@body)))

(define (sum ls) (fold + 0 ls))

(define (length ls)
  (sum (map (lambda (x) 1) ls)))

(define (take ls n)
  (define (rec i ls acc)
    (if (= i 0)
        (reverse acc)
        (rec (- i 1) (cdr ls) (cons (car ls) acc))))
  (rec n ls null))

(define (drop ls n)
  (if (= n 0) ls
      (drop (cdr ls) (- n 1))))

; (define (staged-power n)
;   `(lambda (x)
;      ,(begin (define (rec n)
;                (cond ((= n 0) '1) 
;                      ((= n 1) 'x)
;                      (else `(* x ,(rec (- n 1))))))
;              (rec n))))
; 
; (print (staged-power 5))
; (print (eval (staged-power 5)))
; (print ((eval (staged-power 5)) 3))

; (let ((ls (cdr (iota 101))))
;   (map print
;        (map (lambda (x)
;               (let ((check (lambda (y) (= (% x y) 0))))
;                 (cond ((check 15) 'FizzBuzz)
;                       ((check 3)  'Fizz)
;                       ((check 5)  'Buzz)
;                       (else x))))
;             ls)))

