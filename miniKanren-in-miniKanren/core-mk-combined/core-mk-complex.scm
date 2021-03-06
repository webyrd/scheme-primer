;;; Code from `core-mk-explicit-unification-failure-streams`

;; Relational environment-passing, substitution-passing interpreter
;; for a subset of miniKanren, written in miniKanren.
;;
;; This version of the interpreter explicitly represents failure, in
;; addition to success, and explicitly represents streams.

#|
Grammar:

;; run* expression
run1-expr ::= (run* (<x>) <ge>)

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

(define mzero-complex '())
(define (unit-complex s/c) (cons s/c mzero-complex))

(define (mko-complex expr out)
  (fresh (q ge $)
    (== `(run* (,q) ,ge) expr)
    (symbolo q)
    (eval-mko-complex
     ;; goal expression
     ge
     ;; initial env
     `((,q . (var z)))
     ;; initial s/c
     '(() . (s z))
     ;; resulting stream of s/c's
     $)
    (map-walk*o-complex `(var z) $ out)))

(define (eval-mko-complex expr env s/c $)
  (conde
    ((fresh (e1 e2 t1 t2 s s^ c)
       (== `(== ,e1 ,e2) expr)
       (== `(,s . ,c) s/c)
       (evalo-complex e1 env t1)
       (evalo-complex e2 env t2)
       (conde
         ((== #f s^) (== mzero-complex $))
         ((=/= #f s^) (== (unit-complex `(,s^ . (s ,c))) $)))
       (unifyo-complex t1 t2 s s^)))
    ((fresh (x ge s c)
       (== `(fresh (,x) ,ge) expr)
       (symbolo x)
       (== `(,s . ,c) s/c)
       (eval-mko-complex ge `((,x . (var ,c)) . ,env) `(,s . (s ,c)) $)))
    ((fresh (x ge1 ge2 env^ s c $^)
       (== `(fresh (,x) ,ge1 ,ge2) expr)
       (symbolo x)
       (== `(,s . ,c) s/c)
       (== `((,x . (var ,c)) . ,env) env^)
       (eval-mko-complex ge1 env^ `(,s . (s ,c)) $^)
       (bindo-complex $^ ge2 env^ $)))
    ((fresh (ge1 ge2 $1 $2)
       (== `(conde (,ge1) (,ge2)) expr)
       (eval-mko-complex ge1 env s/c $1)
       (eval-mko-complex ge2 env s/c $2)
       (mpluso-complex $1 $2 $)))))

(define (mpluso-complex $1 $2 $^)
  (conde
    ((== '() $1) (== $2 $^))
    ;;
    ;; no procedure/delayed clause
    ;;
    ;; TODO: do we need the procedure/delayed clause?  If so, how to
    ;; implement it?  Using `evalo-complex`?
    ;;
    ((fresh (a d res)
       (== `(,a . ,d) $1)
       (== `(,a . ,res) $^)
       (mpluso-complex d $2 res)))))

(define (bindo-complex $ g env $^)
  (conde
    ((== '() $) (== mzero-complex $^))
    ;;
    ;; no procedure/delayed clause
    ;;
    ;; TODO: do we need the procedure/delayed clause?  If so, how to
    ;; implement it?  Using `evalo-complex`?
    ;;
    ((fresh (a d $1^ $2^)
       (== `(,a . ,d) $)
       ;; we need to use `eval-mko-complex`, which takes `env`,
       ;; to keep everything relational
       (eval-mko-complex g env a $1^)
       (bindo-complex d g env $2^)
       (mpluso-complex $1^ $2^ $^)))))

(define (evalo-complex expr env val)
  (conde
    ((== `(quote ,val) expr)
     (absento 'var val))
    ((symbolo expr) (lookupo-complex expr env val))
    ((fresh (e1 e2 v1 v2)
       (== `(cons ,e1 ,e2) expr)
       (== `(,v1 . ,v2) val)
       (evalo-complex e1 env v1)
       (evalo-complex e2 env v2)))))

(define (lookupo-complex x env val)
  (fresh (y v rest)
    (== `((,y . ,v) . ,rest) env)
    (conde
      ((== x y) (== v val))
      ((=/= x y)
       (lookupo-complex x rest val)))))

(define (unifyo-complex t1 t2 subst subst^)
  (fresh (t1^ t2^)
    (walko-complex t1 subst t1^)
    (walko-complex t2 subst t2^)
    (conde
      ;; ----- symbols -------
      ;; symbol with symbol
      ((symbolo t1^) (symbolo t2^)
       (conde
         ((== t1^ t2^) (== subst subst^))
         ((=/= t1^ t2^) (== #f subst^))))
      ;; symbol with empty list
      ((symbolo t1^) (== '() t2^)
       (== #f subst^))
      ((symbolo t2^) (== '() t1^)
       (== #f subst^))
      ;; symbol with pair
      ((fresh (a2 d2)
         (symbolo t1^)
         (== `(,a2 . ,d2) t2^)
         (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
         (== #f subst^)))
      ((fresh (a1 d1)
         (symbolo t2^)
         (== `(,a1 . ,d1) t1^)
         (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
         (== #f subst^)))
      ;; symbol with var
      ((fresh (c1)
         (== `(var ,c1) t1^)
         (symbolo t2^) ;; t2^ is a literal symbol, not a var
         (== `(((var ,c1) . ,t2^) . ,subst) subst^)))
      ((fresh (c2)
         (== `(var ,c2) t2^)
         (symbolo t1^) ;; t1^ is a literal symbol, not a var
         (== `(((var ,c2) . ,t1^) . ,subst) subst^)))
      ;; ----- empty list -------
      ;; empty list with empty list
      ((== '() t1^) (== '() t2^) (== subst subst^))
      ;; empty list with symbol -- handled above
      ;; empty list with pair
      ((fresh (a2 d2)
         (== '() t1^)
         (== `(,a2 . ,d2) t2^)
         (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
         (== #f subst^)))
      ((fresh (a1 d1)
         (== '() t2^)
         (== `(,a1 . ,d1) t1^)
         (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
         (== #f subst^)))        
      ;; empty list with var
      ((fresh (c1)
         (== `(var ,c1) t1^)
         (== '() t2^)
         (== `(((var ,c1) . ,t2^) . ,subst) subst^)))
      ((fresh (c2)
         (== `(var ,c2) t2^)
         (== '() t1^)
         (== `(((var ,c2) . ,t1^) . ,subst) subst^)))
      ;; ----- var -------
      ;; var and var
      ((fresh (c1 c2)
         (== `(var ,c1) t1^)
         (== `(var ,c2) t2^)
         (conde
           ((== c1 c2) (== subst subst^))
           ((=/= c1 c2) (== `(((var ,c1) . (var ,c2)) . ,subst) subst^)))))
      ;; var with symbol -- handled above
      ;; var with empty list -- handled above
      ;; var with pair       
      ((fresh (c1 a2 d2)
         (== `(var ,c1) t1^)
         (== `(,a2 . ,d2) t2^)
         (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
         (conde
           ((== `(((var ,c1) . (,a2 . ,d2)) . ,subst) subst^)
            (not-occurso-complex t1^ t2^))
           ((== #f subst^)
            (occurso-complex t1^ t2^)))))
      ((fresh (c2 a1 d1)
         (== `(var ,c2) t2^)
         (== `(,a1 . ,d1) t1^)
         (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
         (conde
           ((== `(((var ,c2) . (,a1 . ,d1)) . ,subst) subst^)
            (not-occurso-complex t2^ t1^))
           ((== #f subst^)
            (occurso-complex t2^ t1^)))))
      ;; ----- pair -------
      ;; pair with pair
      ((fresh (a1 d1 a2 d2 subst^^)
         (== `(,a1 . ,d1) t1^)
         (== `(,a2 . ,d2) t2^)
         (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
         (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
         (unifyo-complex a1 a2 subst subst^^)           
         (conde
           ((== #f subst^^)
            (== #f subst^))
           ((=/= #f subst^^)
            (unifyo-complex d1 d2 subst^^ subst^)))))
      ;; pair with symbol -- handled above
      ;; pair with empty list -- handled above
      ;; pair with var -- handled above
      )))

(define (occurso-complex x t)
  (fresh (c)
    (== `(var ,c) x)
    (conde
      ((== `(var ,c) t))
      ((fresh (a d)
         (== `(,a . ,d) t)
         (=/= 'var a) ;; don't mistake tagged vars for regular pairs
         (conde
           ((occurso-complex x a))
           ((not-occurso-complex x a)
            (occurso-complex x d))))))))

(define (not-occurso-complex x t)
  (fresh (c)
    (== `(var ,c) x)
    (conde
      ((symbolo t))
      ((== '() t))
      ((fresh (c^)
         (== `(var ,c^) t)
         (=/= c c^)))
      ((fresh (a d)
         (== `(,a . ,d) t)
         (=/= 'var a) ;; don't mistake tagged vars for regular pairs
         (not-occurso-complex x a)
         (not-occurso-complex x d))))))

(define (walko-complex t subst t^)
  (letrec ((walk-varo
            (lambda (t s t^)
              (conde
                ((== '() s) (== t t^))
                ((fresh (c u rest)
                   (== `(((var ,c) . ,u) . ,rest) s)
                   (conde
                     ((== `(var ,c) t) (walko-complex u subst t^))
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
         (walk-varo t subst t^))))))

(define (walk*o-complex t subst t^)
  (fresh (t^^)
    (walko-complex t subst t^^)
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
         (walk*o-complex a subst a^)
         (walk*o-complex d subst d^))))))

(define (map-walk*o-complex t $ out)
  (conde
    ((== '() $) (== '() out))
    ((fresh (s c rest res t^)
       (== `((,s . ,c) . ,rest) $)
       (== `(,t^ . ,res) out)
       (walk*o-complex t s t^)
       (map-walk*o-complex t rest res)))))
