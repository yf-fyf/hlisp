(_define (quote define) (macro (id body)
  ; `(_define (quote ,id) body)
  (list (quote _define) (list (quote quote) id) body)))

(define define-macro (macro (name params body)
  ; `(define ,name (macro ,params ,body))
  (list (quote define) name
        (list (quote macro) params body))))

(define-macro set! (id body)
  (list (quote _set!) (list (quote quote) id) body))

(define-macro defun (name params body)
  ; `(begin (define ,name nil) (set! ,name (lambda ,params ,body)))
  (list (quote begin)
        (list (quote define) name nil)
        (list (quote set!) name
              (list (quote lambda) params
                    body))))

(define nil ())
(defun nil? (x) (eq? x nil))

(define-macro delay (x)
  ; `(lambda () ,x)
  (list (quote lambda) () x))
(defun force (f) (f))
(define-macro if (x y z)
  ; `(force (_if ,x (delay ,y) (delay ,z)))
  (list (quote force)
        (list (quote _if) x
              (list (quote delay) y)
              (list (quote delay) z))))
