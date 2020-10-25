(load "../../faster-miniKanren/mk-vicare.scm")
(load "../../faster-miniKanren/mk.scm")

;; Relational environment-passing interpreter for a subset of
;; miniKanren, written in miniKanren.
;;
;; The `mko` driver relation simulates non-deterministic evaluation of
;; a `run 1` expression (not a `run*` expression!), as a single value
;; is associated with the query variable upon success.
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

;; Logic variables are represented metacircularly, as regular
;; miniKanren logic variables in the host miniKanren.

(define mko
  (lambda (expr out)
    (fresh (q ge)
      (== `(run 1 (,q) ,ge) expr)
      (symbolo q)
      (eval-mko ge `((,q . ,out))))))

(define eval-mko
  (lambda (expr env)
    (conde
      ((fresh (e1 e2 t)
         (== `(== ,e1 ,e2) expr)
         (evalo e1 env t)
         (evalo e2 env t)))
      ((fresh (x x^ ge)
         (== `(fresh (,x) ,ge) expr)
         (symbolo x)
         (eval-mko ge `((,x . ,x^) . ,env))))
      ((fresh (x x^ ge1 ge2)
         (== `(fresh (,x) ,ge1 ,ge2) expr)
         (symbolo x)
         (eval-mko ge1 `((,x . ,x^) . ,env))
         (eval-mko ge2 `((,x . ,x^) . ,env))))
      ((fresh (ge1 ge2)
         (== `(conde (,ge1) (,ge2)) expr)
         (conde
           ((eval-mko ge1 env))
           ((eval-mko ge2 env))))))))

(define evalo
  (lambda (expr env val)
    (conde
      ((== `(quote ,val) expr))
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
