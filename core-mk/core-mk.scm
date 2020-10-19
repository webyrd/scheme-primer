(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

;; TODO:
;;
;; fix variables by adding a counter to make them unique
;; add occur check
;;
;; support =/=, symbolo, numbero, and absento
;; support helpers and recursion

(define mko
  (lambda (expr out)
    (fresh (q e subst^)
      (== `(run* (,q) ,e) expr)
      (symbolo q)
      (eval-mko e `((,q . (var ,q))) '() subst^)
      (walk*o `(var ,q) subst^ out))))

(define eval-mko
  (lambda (expr env subst subst^)
    (conde
      ((fresh (e1 e2 t1 t2)
         (== `(== ,e1 ,e2) expr)
         (evalo e1 env t1)
         (evalo e2 env t2)
         (unifyo t1 t2 subst subst^)))
      ((fresh (x e subst^^)
         (== `(fresh (,x) ,e) expr)
         (symbolo x)
         (eval-mko e `((,x . (var ,x))) subst subst^)))
      ((fresh (x e1 e2 subst^^)
         (== `(fresh (,x) ,e1 ,e2) expr)
         (symbolo x)
         (eval-mko e1 `((,x . (var ,x))) subst subst^^)
         (eval-mko e2 `((,x . (var ,x))) subst^^ subst^)))
      ((fresh (e1 e2)
         (== `(conde (,e1) (,e2)) expr)
         (conde
           ((eval-mko e1 env subst subst^))
           ((eval-mko e2 env subst subst^))))))))

(define evalo
  (lambda (expr env val)
    (conde
      ((== `(quote ,val) expr) (absento 'var val))
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
        ((fresh (x1 x2)
           (== `(var ,x1) t1^)
           (== `(var ,x2) t2^)
           (symbolo x1)
           (symbolo x2)
           (== `(((var ,x1) . (var ,x2)) . ,subst) subst^)))
        ((fresh (x1)
           (== `(var ,x1) t1^)
           (symbolo x1)
           (symbolo t2^) ;; t2^ is a literal symbol, not a var
           (== `(((var ,x1) . ,t2^) . ,subst) subst^)))
        ((fresh (x2)
           (== `(var ,x2) t2^)
           (symbolo x2)
           (symbolo t1^) ;; t1^ is a literal symbol, not a var
           (== `(((var ,x2) . ,t1^) . ,subst) subst^)))
        ((fresh (x1 a2 d2)
           (== `(var ,x1) t1^)
           (symbolo x1)
           (== `(,a2 . ,d2) t2^)
           (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
           (== `(((var ,x1) . (,a2 . ,d2)) . ,subst) subst^)))
        ((fresh (x2 a1 d1)
           (== `(var ,x2) t2^)
           (symbolo x2)
           (== `(,a1 . ,d1) t1^)
           (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
           (== `(((var ,x2) . (,a1 . ,d1)) . ,subst) subst^)))
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
                  ((fresh (y u rest)
                     (== `(((var ,y) . ,u) . ,rest) s)
                     (symbolo y)
                     (conde
                       ((== `(var ,y) t) (walko u subst t^))
                       ((=/= `(var ,y) t) (walk-varo t rest t^)))))))))
      (conde
        ((symbolo t) (== t t^))
        ((fresh (a d)
           (== `(,a . ,d) t)
           (=/= 'var a) ;; don't mistake tagged vars for regular pairs
           (== t t^)))
        ((fresh (x)
           (== `(var ,x) t)
           (symbolo x)
           (walk-varo t subst t^)))))))

(define walk*o
  (lambda (t subst t^)
    (fresh (t^^)
      (walko t subst t^^)
      (conde
        ((symbolo t^^) (== t^^ t^))
        ((fresh (x)
           (== `(var ,x) t^^)
           (symbolo x)
           (== t^^ t^)))
        ((fresh (a d a^ d^)
           (== `(,a . ,d) t^^)
           (=/= 'var a) ;; don't mistake tagged vars for regular pairs
           (== `(,a^ . ,d^) t^)
           (walk*o a subst a^)
           (walk*o d subst d^)))))))

(run* (q) (walko 'dog '(((var x) . cat)) q))

(run* (q) (walko '(var x) '(((var x) . cat)) q))

(run 1 (expr subst^)
  (eval-mko expr '() '() subst^))

(run* (subst^)
  (eval-mko '(== 'cat 'cat) '() '() subst^))

(run* (subst^)
  (eval-mko '(== 'cat 'dog) '() '() subst^))

(run* (subst^)
  (eval-mko '(fresh (x) (== x 'cat) (== 'dog 'dog)) '() '() subst^))

(run* (subst^)
  (eval-mko '(fresh (x) (== x 'cat) (== 'cat x)) '() '() subst^))

(run* (subst^)
  (eval-mko '(fresh (x) (== x 'cat) (== 'dog x)) '() '() subst^))

(run* (subst^)
  (eval-mko '(fresh (x) (conde ((== x 'cat)) ((== x 'dog)))) '() '() subst^))

(run* (subst^)
  (eval-mko '(fresh (x) (conde ((== 'cat 'cat)) ((== x 'dog)))) '() '() subst^))

(run* (subst^)
  (eval-mko '(conde ((== 'cat 'cat)) ((== 'dog 'dog))) '() '() subst^))

(run 2 (expr)
  (eval-mko expr '() '() '(((var x) . dog))))
;; =>
#|
((fresh (x) (== 'dog x)) (fresh (x) (== x 'dog)))
|#

(run 10 (expr)
  (eval-mko expr '() '() '(((var x) . dog))))
;; =>
#|
((fresh (x) (== 'dog x)) (fresh (x) (== x 'dog))
 ((fresh (_.0) (fresh (x) (== 'dog x))) (sym _.0))
 ((fresh (x) (== '_.0 '_.0) (== 'dog x))
  (=/= ((_.0 var)))
  (sym _.0))
 ((fresh (_.0) (fresh (x) (== x 'dog))) (sym _.0))
 ((fresh (x) (== '_.0 '_.0) (== x 'dog))
  (=/= ((_.0 var)))
  (sym _.0))
 ((fresh (x) (== 'dog x) (== '_.0 '_.0))
  (=/= ((_.0 var)))
  (sym _.0))
 ((fresh (x) (fresh (_.0) (== '_.1 '_.1)) (== 'dog x))
  (=/= ((_.1 var)))
  (sym _.0 _.1))
 ((fresh (x) (fresh (_.0) (== '_.1 '_.1)) (== x 'dog))
  (=/= ((_.1 var)))
  (sym _.0 _.1))
 ((fresh (x) (== '(_.0 . dog) (cons '_.0 x)))
  (=/= ((_.0 var)))
  (sym _.0)))
|#

(run* (q) (mko '(run* (x) (== x 'cat)) q))

(run* (q) (mko '(run* (x) (conde ((== x 'cat)) ((== 'dog x)))) q))

