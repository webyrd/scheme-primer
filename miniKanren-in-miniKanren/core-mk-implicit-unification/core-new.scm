(load "../../faster-miniKanren/mk-vicare.scm")
(load "../../faster-miniKanren/mk.scm")
(load "../../faster-miniKanren/test-check.scm")

;; Experimenting with a new simple foundation for
;; miniKanren-in-miniKanren, meta-circularly, as the basis for
;; reflective towers, relational style.

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
       (eval-condeo (cons c c*) env)))))

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
       (eval-listo e* env val)))))

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
