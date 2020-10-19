(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

;; TODO:
;;
;; make reification work like in regular my by using some kind of fake subst
;;
;; add occur check
;;
;; support =/=, symbolo, numbero, and absento
;; support helpers and recursion

(define mko
  (lambda (expr out)
    (fresh (q e count^ subst^)
      (== `(run* (,q) ,e) expr)
      (symbolo q)
      (eval-mko e `((,q . (var z))) `(s z) count^ '() subst^)
      (walk*o `(var z) subst^ out))))

(define eval-mko
  (lambda (expr env count count^ subst subst^)
    (conde
      ((fresh (e1 e2 t1 t2)
         (== `(== ,e1 ,e2) expr)
         (evalo e1 env t1)
         (evalo e2 env t2)
         (unifyo t1 t2 subst subst^)))
      ((fresh (x e subst^^)
         (== `(fresh (,x) ,e) expr)
         (symbolo x)
         (eval-mko e `((,x . (var ,count))) `(s ,count) count^ subst subst^)))
      ((fresh (x e1 e2 count^^ subst^^)
         (== `(fresh (,x) ,e1 ,e2) expr)
         (symbolo x)
         (eval-mko e1 `((,x . (var ,count))) `(s ,count) count^^ subst subst^^)
         (eval-mko e2 `((,x . (var ,count))) count^^ count^ subst^^ subst^)))
      ((fresh (e1 e2)
         (== `(conde (,e1) (,e2)) expr)
         (conde
           ((eval-mko e1 env count count^ subst subst^))
           ((eval-mko e2 env count count^ subst subst^))))))))

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
        ((fresh (c1 c2)
           (== `(var ,c1) t1^)
           (== `(var ,c2) t2^)
           (== `(((var ,c1) . (var ,c2)) . ,subst) subst^)))
        ((fresh (c1)
           (== `(var ,c1) t1^)
           (symbolo t2^) ;; t2^ is a literal symbol, not a var
           (== `(((var ,c1) . ,t2^) . ,subst) subst^)))
        ((fresh (c2)
           (== `(var ,c2) t2^)
           (symbolo t1^) ;; t1^ is a literal symbol, not a var
           (== `(((var ,c2) . ,t1^) . ,subst) subst^)))
        ((fresh (c1 a2 d2)
           (== `(var ,c1) t1^)
           (== `(,a2 . ,d2) t2^)
           (=/= 'var a2) ;; don't mistake tagged vars for regular pairs
           (== `(((var ,c1) . (,a2 . ,d2)) . ,subst) subst^)))
        ((fresh (c2 a1 d1)
           (== `(var ,c2) t2^)
           (== `(,a1 . ,d1) t1^)
           (=/= 'var a1) ;; don't mistake tagged vars for regular pairs
           (== `(((var ,c2) . (,a1 . ,d1)) . ,subst) subst^)))
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
        ((fresh (c)
           (== `(var ,c) t^^)
           (== t^^ t^)))
        ((fresh (a d a^ d^)
           (== `(,a . ,d) t^^)
           (=/= 'var a) ;; don't mistake tagged vars for regular pairs
           (== `(,a^ . ,d^) t^)
           (walk*o a subst a^)
           (walk*o d subst d^)))))))

(run* (q) (walko 'dog '(((var z) . cat)) q))

(run* (q) (walko '(var z) '(((var z) . cat)) q))

(run* (q) (walko '(var (s (s (s z)))) '(((var (s (s (s z)))) . cat)) q))

(run* (q) (walko '(var (s (s (s z)))) '(((var (s (s z))) . cat)) q))


(run 1 (expr subst^)
  (fresh (c)
    (eval-mko expr '() '() 'z c subst^)))

(run* (subst^)
  (fresh (c)
    (eval-mko '(== 'cat 'cat) '() 'z c '() subst^)))

(run* (subst^)
  (fresh (c)
    (eval-mko '(== 'cat 'dog) '() 'z c '() subst^)))

(run* (subst^)
  (fresh (c)
    (eval-mko '(fresh (x) (== x 'cat) (== 'dog 'dog)) '() 'z c '() subst^)))


(run* (subst^)
  (fresh (c)
    (eval-mko '(fresh (x) (== x 'cat) (== 'cat x)) '() 'z c '() subst^)))

(run* (subst^)
  (fresh (c)
    (eval-mko '(fresh (x) (== x 'cat) (== 'dog x)) '() 'z c '() subst^)))

(run* (subst^)
  (fresh (c)
    (eval-mko '(fresh (x) (conde ((== x 'cat)) ((== x 'dog)))) '() 'z c '() subst^)))


(run* (subst^)
  (fresh (c)
    (eval-mko '(fresh (x) (conde ((== 'cat 'cat)) ((== x 'dog)))) '() 'z c '() subst^)))

(run* (subst^)
  (fresh (c)
    (eval-mko '(conde ((== 'cat 'cat)) ((== 'dog 'dog))) '() 'z c '() subst^)))

(run 2 (expr)
  (fresh (c)
    (eval-mko expr '() 'z c '() '(((var z) . dog)))))
;; =>
#|
(((fresh (_.0) (== 'dog _.0)) (sym _.0))
 ((fresh (_.0) (== _.0 'dog)) (sym _.0)))
|#

(run 10 (expr)
  (fresh (c)
    (eval-mko expr '() 'z c '() '(((var z) . dog)))))
;; =>
#|
(((fresh (_.0) (== 'dog _.0))
  (sym _.0))
 ((fresh (_.0) (== _.0 'dog))
  (sym _.0))
 ((fresh (_.0)
    (== '_.1 '_.1)
    (== 'dog _.0))
  (=/= ((_.1 var)))
  (sym _.0 _.1))
 ((fresh (_.0)
    (== '_.1 '_.1)
    (== _.0 'dog))
  (=/= ((_.1 var)))
  (sym _.0 _.1))
 ((fresh (_.0)
    (== 'dog _.0)
    (== '_.1 '_.1))
  (=/= ((_.1 var)))
  (sym _.0 _.1))
 ((fresh (_.0)
    (fresh (_.1)
      (== '_.2 '_.2))
    (== 'dog _.0))
  (=/= ((_.2 var)))
  (sym _.0 _.1 _.2))
 ((fresh (_.0)
    (fresh (_.1)
      (== '_.2 '_.2))
    (== _.0 'dog))
  (=/= ((_.2 var)))
  (sym _.0 _.1 _.2))
 ((fresh (_.0)
    (== '(_.1 . dog) (cons '_.1 _.0)))
  (=/= ((_.1 var)))
  (sym _.0 _.1))
 ((fresh (_.0)
    (== 'dog _.0)
    (fresh (_.1)
      (== '_.2 '_.2)))
  (=/= ((_.2 var)))
  (sym _.0 _.1 _.2))
 ((fresh (_.0)
    (conde
      ((== 'dog _.0))
      (_.1)))
  (sym _.0)))
|#

(run* (q) (mko '(run* (x) (== x 'cat)) q))

(run* (q) (mko '(run* (x) (conde ((== x 'cat)) ((== 'dog x)))) q))
