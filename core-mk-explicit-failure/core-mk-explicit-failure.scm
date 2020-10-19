(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

;; Relational environment-passing, substitution-passing interpreter
;; for a subset of miniKanren, written in miniKanren.
;;
;; This version of the interpreter explicitly represents failure
;; (represented as #f), in addition to success (represented as a
;; substitution)
;;
;; The `mko` driver relation simulates non-deterministic evaluation of
;; a `run 1` expression (not a `run*` expression!).
;;
;; In order to simulate full `run*` behavior, I think it would be
;; necessary to implement the notions of success and failure
;; explicitly.  This implementation represents failure metacircularly,
;; as failure at the host-level miniKanren.  Similarly, this
;; interpreter represents `conde` and the miniKanren search
;; metacircularly, using the host `conde`.  There seems to be tradeoff
;; in expressiveness vs. convenience: this interpreter can't express
;; that a miniKanren program *doesn't* produce a certain answer, for
;; example.  Also, this interpreter can't be used to reason about
;; setof/bagof-style 2nd order relations, since the "collected" answers
;; are collected through the host `run`, rather than through a `run*`
;; in the object miniKanren.  Indeed, the object `run` can only express
;; (non-deterministic) `run 1` semantics, rather than `run*` semantics.

#|
Grammar:

;; run 1 expression
run1-expr ::= (run 1 (<x>) <ge>)

;; goal expression
<ge> ::= (== <e> <e>) |
         (fresh (<x>) <ge>) |
         (fresh (<x>) <ge> <ge>) |
         (conde (<ge>) (<ge>))

;; Scheme expression
<e> ::= <x> |
        (quote <datum>) |
        (cons <e> <e>)

;; Scheme lexical variable
<x> ::= <symbol>

;; quoted datum
<datum> ::= <symbol> |
            () |
            (<datum> . <datum>)
|#

;; Logic variables are represented as tagged lists of the form `(var ,c)`
;; where `c` is a Peano numeral of the form `z`, `(s z)`, `(s (s z))`, etc.
;; Logic variables that remain fresh are reified as themselves, rather than
;; being replaced with `_.0`, `_.1`, etc.


;; TODO:
;;
;; Think about reification of fresh logic variables--should it work
;; like in regular mk, by using some kind of fake subst?  If so, would
;; you be able to tell whether '_.0' came from miniKanren, or the
;; language being interpreted?
;;
;; Support =/=, symbolo, numbero, and absento
;;
;; Support helpers and recursion

(define mko
  (lambda (expr out)
    (fresh (q ge count^ subst^ t)
      (== `(run 1 (,q) ,ge) expr)
      (symbolo q)
      (== (list t) out)
      (eval-mko ge `((,q . (var z))) `(s z) count^ '() subst^)
      (walk*o `(var z) subst^ t))))

(define eval-mko
  (lambda (expr env count count^ subst subst^)
    (conde
      ((fresh (e1 e2 t1 t2)
         (== `(== ,e1 ,e2) expr)
         (evalo e1 env t1)
         (evalo e2 env t2)
         (unifyo t1 t2 subst subst^)))
      ((fresh (x ge subst^^)
         (== `(fresh (,x) ,ge) expr)
         (symbolo x)
         (eval-mko ge `((,x . (var ,count)) . ,env) `(s ,count) count^ subst subst^)))
      ((fresh (x ge1 ge2 count^^ subst^^)
         (== `(fresh (,x) ,ge1 ,ge2) expr)
         (symbolo x)
         (eval-mko ge1 `((,x . (var ,count)) . ,env) `(s ,count) count^^ subst subst^^)
         (eval-mko ge2 `((,x . (var ,count)) . ,env) count^^ count^ subst^^ subst^)))
      ((fresh (ge1 ge2)
         (== `(conde (,ge1) (,ge2)) expr)
         (conde
           ((eval-mko ge1 env count count^ subst subst^))
           ((eval-mko ge2 env count count^ subst subst^))))))))

(define evalo
  (lambda (expr env val)
    (conde
      ((== `(quote ,val) expr)
       (absento 'var val))
      ((symbolo expr) (lookupo expr env val))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo e1 env v1)
         (evalo e2 env v2))))))

(define lookupo
  (lambda (x env val)
    (fresh (y v rest)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== x y) (== v val))
        ((=/= x y)
         (lookupo x rest val))))))

(define unifyo
  (lambda (t1 t2 subst subst^)
    (fresh (t1^ t2^)
      (walko t1 subst t1^)
      (walko t2 subst t2^)
      (conde
        ((symbolo t1^) (symbolo t2^) (== t1^ t2^) (== subst subst^))
        ((== '() t1^) (== '() t2^) (== subst subst^))
        ((fresh (c1 c2)
           (== `(var ,c1) t1^)
           (== `(var ,c2) t2^)
           (conde
             ((== c1 c2) (== subst subst^))
             ((=/= c1 c2) (== `(((var ,c1) . (var ,c2)) . ,subst) subst^)))))
        ((fresh (c1)
           (== `(var ,c1) t1^)
           (symbolo t2^) ;; t2^ is a literal symbol, not a var
           (== `(((var ,c1) . ,t2^) . ,subst) subst^)))
        ((fresh (c2)
           (== `(var ,c2) t2^)
           (symbolo t1^) ;; t1^ is a literal symbol, not a var
           (== `(((var ,c2) . ,t1^) . ,subst) subst^)))
        ((fresh (c1)
           (== `(var ,c1) t1^)
           (== '() t2^)
           (== `(((var ,c1) . ,t2^) . ,subst) subst^)))
        ((fresh (c2)
           (== `(var ,c2) t2^)
           (== '() t1^)
           (== `(((var ,c2) . ,t1^) . ,subst) subst^)))
        ((fresh (c1 a2 d2)
           (== `(var ,c1) t1^)
           (== `(,a2 . ,d2) t2^)
           (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
           (== `(((var ,c1) . (,a2 . ,d2)) . ,subst) subst^)
           (absento t1^ t2^) ;; use absento to implement the occurs check
           ))
        ((fresh (c2 a1 d1)
           (== `(var ,c2) t2^)
           (== `(,a1 . ,d1) t1^)
           (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
           (== `(((var ,c2) . (,a1 . ,d1)) . ,subst) subst^)
           (absento t2^ t1^) ;; use absento to implement the occurs check
           ))
        ((fresh (a1 d1 a2 d2 subst^^)
           (== `(,a1 . ,d1) t1^)
           (== `(,a2 . ,d2) t2^)
           (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
           (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
           (unifyo a1 a2 subst subst^^)
           (unifyo d1 d2 subst^^ subst^)))))))

(define walko
  (lambda (t subst t^)
    (letrec ((walk-varo
              (lambda (t s t^)
                (conde
                  ((== '() s) (== t t^))
                  ((fresh (c u rest)
                     (== `(((var ,c) . ,u) . ,rest) s)
                     (conde
                       ((== `(var ,c) t) (walko u subst t^))
                       ((=/= `(var ,c) t) (walk-varo t rest t^)))))))))
      (conde
        ((symbolo t) (== t t^))
        ((== '() t) (== t t^))
        ((fresh (a d)
           (== `(,a . ,d) t)
           (=/= 'var a) ;; don't mistake tagged vars for regular pairs
           (== t t^)))
        ((fresh (c)
           (== `(var ,c) t)
           (walk-varo t subst t^)))))))

(define walk*o
  (lambda (t subst t^)
    (fresh (t^^)
      (walko t subst t^^)
      (conde
        ((symbolo t^^) (== t^^ t^))
        ((== '() t^^) (== t^^ t^))
        ((fresh (c)
           (== `(var ,c) t^^)
           (== t^^ t^)))
        ((fresh (a d a^ d^)
           (== `(,a . ,d) t^^)
           (=/= 'var a) ;; don't mistake tagged vars for regular pairs
           (== `(,a^ . ,d^) t^)
           (walk*o a subst a^)
           (walk*o d subst d^)))))))
