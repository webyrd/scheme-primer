(load "core-mk.scm")
(load "../faster-miniKanren/test-check.scm")

(run* (q) (mko '(run 1 (x) (== x 'cat)) q))

(run* (q) (mko '(run 1 (x) (conde ((== x 'cat)) ((== 'dog x)))) q))

(run* (q) (mko '(run 1 (x) (== 'cat 'cat)) q))

(run* (q) (mko '(run 1 (x) (fresh (y) (== 'cat 'cat))) q))

(run* (q) (mko '(run 1 (x) (fresh (y) (== x 'cat))) q))

(run* (q) (mko '(run 1 (x) (fresh (x) (== x 'cat))) q))

(run* (q) (mko '(run 1 (x) (fresh (y) (== y 'cat))) q))

(run* (q) (mko '(run 1 (x) (fresh (y) (== y 'cat) (== x y))) q))

(run* (q) (mko '(run 1 (x) (fresh (y) (== x y) (== y 'cat))) q))

(test "mko unify x with itself"
  (run* (q) (mko '(run 1 (x) (== x x)) q))
  '((var z)))

(test "mko unify x with (cons y y)"
  (run* (q)
    (mko '(run 1 (x)
            (fresh (y)
              (== (cons y y) x)))
         q))
  '(((var (s z)) var (s z))))

(test "mko occur-check-1"
  (run* (q) (mko '(run 1 (x) (== (cons x x) x)) q))
  '())

(test "mko occur-check-2"
  (run* (q) (mko '(run 1 (x) (== x (cons x x))) q))
  '())

(test "mko occur-check-3"
  (run* (q)
    (mko '(run 1 (x)
            (fresh (y)
              (== (cons y y) x)
              (== x y)))
         q))
  '())

(test "mko occur-check-4"
  (run* (q)
    (mko '(run 1 (x)
            (fresh (y)
              (== x y)
              (== (cons y y) x)))
         q))
  '())

(test "mko backwards-1"
  (run 10 (e) (mko e 'cat))
  '(((run 1 (_.0) (== 'cat _.0))
     (sym _.0))
    ((run 1 (_.0) (== _.0 'cat))
     (sym _.0))
    ((run 1 (_.0)
       (fresh (_.1)
         (== 'cat _.0)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))
    ((run 1 (_.0)
       (== '(_.1 . cat) (cons '_.1 _.0)))
     (=/= ((_.1 var)))
     (sym _.0 _.1))
    ((run 1 (_.0)
       (conde
         ((== 'cat _.0))
         (_.1)))
     (sym _.0))
    ((run 1 (_.0)
       (conde
         (_.1)
         ((== 'cat _.0))))
     (sym _.0))
    ((run 1 (_.0)
       (== '(cat . _.1) (cons _.0 '_.1)))
     (=/= ((_.1 var)))
     (sym _.0 _.1))
    ((run 1 (_.0)
       (conde
         ((== _.0 'cat))
         (_.1)))
     (sym _.0))
    ((run 1 (_.0)
       (conde
         (_.1)
         ((== _.0 'cat))))
     (sym _.0))
    ((run 1 (_.0)
       (== '((_.1 . _.2) . cat) (cons '(_.1 . _.2) _.0)))
     (=/= ((_.1 var)) ((_.2 var)))
     (sym _.0 _.1 _.2))))

(test "mko backwards-2"
  (run 1 (e)
    (mko e 'cat)
    (mko e 'dog))
  '(((run 1 (_.0)
       (conde
         ((== 'cat _.0))
         ((== 'dog _.0))))
     (sym _.0))))



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

(test "eval-mko backwards-1"
  (run 2 (expr)
    (fresh (c)
      (eval-mko expr '() 'z c '() '(((var z) . dog)))))
  '(((fresh (_.0) (== 'dog _.0))
     (sym _.0))
    ((fresh (_.0) (== _.0 'dog))
     (sym _.0))))

(test "eval-mko backwards-2"
  (run 10 (expr)
    (fresh (c)
      (eval-mko expr '() 'z c '() '(((var z) . dog)))))
  '(((fresh (_.0) (== 'dog _.0)) (sym _.0))
    ((fresh (_.0) (== _.0 'dog)) (sym _.0))
    ((fresh (_.0) (== '_.1 '_.1) (== 'dog _.0))
     (=/= ((_.1 var)))
     (sym _.0 _.1))
    ((fresh (_.0) (== '_.1 '_.1) (== _.0 'dog))
     (=/= ((_.1 var)))
     (sym _.0 _.1))
    ((fresh (_.0) (== 'dog _.0) (== '_.1 '_.1))
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
         (== 'dog _.0)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))
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
     (sym _.0 _.1 _.2))))





(test "walko the dog"
  (run* (q) (walko 'dog '(((var z) . cat)) q))
  '(dog))

(test "walko the dogs"
  (run* (q) (walko '(dog . dog) '(((var z) . cat)) q))
  '((dog . dog)))

(test "walko the vars"
  (run* (q) (walko '((var z) . (var z)) '(((var z) . cat)) q))
  '(((var z) var z)))

(run* (q) (walko '(var z) '(((var z) . cat)) q))

(run* (q) (walko '(var (s (s (s z)))) '(((var (s (s (s z)))) . cat)) q))

(run* (q) (walko '(var (s (s (s z)))) '(((var (s (s z))) . cat)) q))


(test "walk*o the vars"
  (run* (q) (walk*o '((var z) . (var z)) '(((var z) . cat)) q))
  '((cat . cat)))
