(load "../../faster-miniKanren/mk-vicare.scm")
(load "../../faster-miniKanren/mk.scm")
(load "../../faster-miniKanren/test-check.scm")

;; Experimenting with a new simple foundation for
;; miniKanren-in-miniKanren, meta-circularly, as the basis for
;; reflective towers, relational style.
;;
;; With Nada Amin

;; Research Questions:
;;
;; 1. Logic variables are represented metacircularly, as regular
;; miniKanren logic variables in the host miniKanren.  However, the
;; encoding in this file uses an environment that maps logic variables
;; to logic variables.  Is there a meta-circular way to avoid this
;; extra explicit environment?
;;
;; 2. The miniKanren operators are all encoded meta-circularly (== in
;; terms of ==, conde in terms of conde, etc.), *except* for run.  Is
;; there a way to use run metacircularly?  And to get reification via
;; that run?  Or maybe we need a new interface, similar to how
;; reflective towers have 'delta' and 'meaning' operations for
;; reification and reflection.  Inject!
;;
;; In fact, the "fake" run 1 done by mko doesn't do reification--
;; reification is done by the top-level outermost "real" run.
;; Probably the thing done by mko shouldn't be called run at all.

;; Nada suggests 'mu' instead of 'delta', and 'meaning':

;; ((mu (ges env)
;;     (meaning (cadr ges) env))
;;  (== 1 2)
;;  (== 1 1))
;; ignores the first goal, so this succeeds

;; Would env would be the mapping between logic variables?  Do we need
;; a constraint store/state?  Do we want explicit continuations, like
;; in reflective tower languages?
;;
;; (mu (ges env store k) ...)
;; (meaning ges env store k)
;;
;; or perhaps with explicit success and failure continuations:
;;
;; (mu (ges env store sk fk) ...)
;; (meaning ges env store sk fk)
;;
;; Should we be using first-order miniKanren?
;;
;; If we go with sk and fk, should we use a miniKanren based on sk/fk,
;; or can the sk and fk coexist nicely with a streams-based miniKanren?
;;
;; Do mu and meaning need to be implemented in faster-miniKanren, and
;; used meta-circularly?  Nada doesn't think so.
;;
;; mu is a function that, when called, produces a function that is a
;; variadic goal constructor.



;; Logic variables are represented metacircularly, as regular
;; miniKanren logic variables in the host miniKanren.

(define-syntax defrel
  (syntax-rules ()
    [(_ (name x* ...) ge)
     (define (name x* ...) ge)]))

(defrel (mko expr out)
  (fresh (q ge ge*)
    (== (cons 'run (cons 1 (cons (list q) (cons ge ge*)))) expr)
    (symbolo q)
    (eval-conj+o (cons ge ge*) (list (cons q out)))))

(defrel (eval-mko expr env)
  (conde
    ((fresh (e t)
       (== (list 'symbolo e) expr)
       (symbolo t)
       (evalo e env t)))
    ((fresh (e t)
       (== (list 'numbero e) expr)
       (numbero t)
       (evalo e env t)))    
    ((fresh (e1 e2 t)
       (== (list '== e1 e2) expr)
       (evalo e1 env t)
       (evalo e2 env t)))
    ((fresh (e1 e2 t1 t2)
       (== (list '=/= e1 e2) expr)
       (=/= t1 t2)
       (evalo e1 env t1)
       (evalo e2 env t2)))
    ((fresh (x* ge ge*)
       (== (cons 'fresh (cons x* (cons ge ge*))) expr)
       (eval-fresho x* (cons ge ge*) env)))
    ((fresh (c c*)
       (== (list 'conde (cons c c*)) expr)
       (eval-condeo (cons c c*) env)))
    ((fresh (ges-param menv-param mu-body ges-arg evalo-res)
       (== (cons (list 'mu (list ges-param menv-param) mu-body) ges-arg) expr)
       (evalo mu-body (cons (cons ges-param ges-arg) (cons (cons menv-param env) env)) evalo-res)
       (eval-mko evalo-res env)))))

(defrel (eval-fresho x* ge* env)
  (conde
    ((== '() x*)
     (eval-conj+o ge* env))
    ((fresh (x x^ x-rest)
       (== (cons x x-rest) x*)
       (symbolo x)
       (eval-fresho x-rest ge* (cons (cons x x^) env))))))

(defrel (eval-condeo c* env)
  (conde
    ((fresh (c)
       (== (cons c '()) c*)
       (eval-conj+o c env)))
    ((fresh (c0 c1 c-rest)
       (== (cons c0 (cons c1 c-rest)) c*)
       (conde
         ((eval-conj+o c0 env))
         ((eval-condeo (cons c1 c-rest) env)))))))

(defrel (eval-conj+o ge* env)
  (conde
    ((fresh (ge)
       (== (cons ge '()) ge*)
       (eval-mko ge env)))
    ((fresh (ge0 ge1 ge-rest)
       (== (cons ge0 (cons ge1 ge-rest)) ge*)
       (eval-mko ge0 env)
       (eval-conj+o (cons ge1 ge-rest) env)))))

(defrel (evalo expr env val)
  (conde
    ((== (list 'quote val) expr))
    ((numbero expr) (== expr val))
    ((symbolo expr) (lookupo expr env val))
    ((fresh (e1 e2 v1 v2)
       (== (list 'cons e1 e2) expr)
       (== (cons v1 v2) val)
       (evalo e1 env v1)
       (evalo e2 env v2)))
    ((fresh (e*)
       (== (cons 'list e*) expr)
       (eval-listo e* env val)))
    ((fresh (ge menv)
       (== (list 'meaning ge menv) expr)
       (evalo ge menv val)))))

(defrel (eval-listo e* env v*)
  (conde
    ((== '() e*) (== '() v*))
    ((fresh (e e-rest v v-rest)
       (== (cons e e-rest) e*)
       (== (cons v v-rest) v*)
       (evalo e env v)
       (eval-listo e-rest env v-rest)))))

(defrel (lookupo x env val)
  (fresh (y v rest)
    (== (cons (cons y v) rest) env)
    (conde
      ((== x y) (== v val))
      ((=/= x y) (lookupo x rest val)))))


(test "lookupo-1"
  (run* (q) (lookupo 'a '((b . 3) (a . 5) (a . 6)) q))
  '(5))

(test "eval-listo-1"
  (run* (q) (eval-listo '((quote mouse) a 7) '((a . catte)) q))
  '((mouse catte 7)))

(test "evalo-1"
  (run* (q) (evalo '(list (quote mouse) a 7) '((a . catte)) q))
  '((mouse catte 7)))

(test "evalo-1"
  (run* (q) (evalo '(list (cons (quote mouse) (quote ())) a 7) '((a . catte)) q))
  '(((mouse) catte 7)))

(test "mko-1"
  (run* (a) (mko '(run 1 (b) (== 42 b)) a))
  '(42))

(test "mko-2"
  (run* (a) (mko '(run 1 (b) (== (quote cat) b)) a))
  '(cat))

(test "mko-3"
  (run* (a) (mko '(run 1 (b) (symbolo b) (== (quote cat) b)) a))
  '(cat))

(test "mko-4"
  (run* (a) (mko '(run 1 (b) (symbolo b)) a))
  '((_.0 (sym _.0))))

(test "mko-5"
  (run* (a) (mko '(run 1 (b) (numbero b)) a))
  '((_.0 (num _.0))))

(test "mko-6"
  (run* (a) (mko '(run 1 (b) (symbolo b) (numbero b)) a))
  '())

(test "mko-6a"
 (run* (a) (mko '(run 1 (b) (== 5 5)) a))
  '(_.0))

(test "mko-6b"
 (run* (a) (mko '(run 1 (b) (== 5 5)) 42))
  '(_.0))

(test "mko-free-1"
  (length (run 10000 (a b) (mko a b)))
  10000)

(test "mko-free-2"
  (run 10 (a b) (mko a b))
  '((((run 1 (_.0) (symbolo '_.1)) _.2)
     (sym _.0 _.1))
    (((run 1 (_.0) (numbero '_.1)) _.2)
     (num _.1)
     (sym _.0))
    (((run 1 (_.0) (numbero _.1)) _.2)
     (num _.1)
     (sym _.0))
    (((run 1 (_.0) (symbolo _.0)) _.1)
     (sym _.0 _.1))
    (((run 1 (_.0) (numbero _.0)) _.1)
     (num _.1)
     (sym _.0))
    (((run 1 (_.0) (== '_.1 '_.1)) _.2)
     (sym _.0))
    (((run 1 (_.0) (symbolo '_.1) (symbolo '_.2)) _.3)
     (sym _.0 _.1 _.2))
    (((run 1 (_.0) (== '_.1 _.1)) _.2)
     (num _.1)
     (sym _.0))
    (((run 1 (_.0) (== _.1 '_.1)) _.2)
     (num _.1)
     (sym _.0))
    (((run 1 (_.0) (=/= '_.1 '_.2)) _.3)
     (=/= ((_.1 _.2)))
     (sym _.0))))


(test "meta-0"
  (run* (a) (mko '(run 1 (b) ((mu (ges env) (meaning (car ges) env)) (== 1 2) (== 1 1))) a))
  '())

(test "meta-1"
  (run* (a) (mko '(run 1 (b) ((mu (ges env) (meaning (car (cdr ges)) env)) (== 1 2) (== 1 1))) a))
  '(_.0))
