(load "core-mk-explicit-failure-streams.scm")
(load "../faster-miniKanren/test-check.scm")

(test "mko-0"
  (run* (q)
    (mko '(run* (x)
            (fresh (y)
              (== (cons y y) x)))
         q))
  '((((var (s z)) . (var (s z))))))

(test "mko-1"
  (run* (q) (mko '(run* (x)
                    (== x 'cat))
                 q))
  '((cat)))

;; run 2 diverges (didn't diverge when we didn't support explicit failure)
;;
;; can we do better?
(test "mko-1b"
  (run 1 (e)
    (mko `(run* (x)
            (== ',e x))
         '(cat)))
  '(cat))

;; `run 2` appears to diverge, which is expected, since there are
;; infinitely many expressions `e`, and only 'cat will satisfy the
;; `==` constraint.
(test "mko-1c"
  (run 1 (e)
    (mko `(run* (x)
            (== ,e x))
         '(cat)))
  '('cat))

(test "mko occur check violation-1"
  (run* (q)
    (mko `(run* (x)
            (== (cons x x) x))
         q))
  '(()))

(test "mko occur check violation-2"
  (run* (q)
    (mko `(run* (x)
            (== (cons x x) x))
         '()))
  '(_.0))

(test "mko fail backwards-0"
  (run 1 (e)
    (mko `(run* (x)
            (== (cons ,e ,e) x))
         '()))
  '(x))

(test "mko fail backwards-1"
  (run 8 (e)
    (mko `(run* (x)
            (== ,e x))
         '()))
  '(((cons '_.0 x) (=/= ((_.0 var))) (sym _.0))
    (cons '() x)
    ((cons '(_.0 . _.1) x)
     (=/= ((_.0 var)) ((_.1 var)))
     (sym _.0 _.1))
    ((cons '(_.0) x)
     (=/= ((_.0 var))) (sym _.0))
    ((cons '(() . _.0) x)
     (=/= ((_.0 var))) (sym _.0))
    (cons '(()) x)
    ((cons x '_.0) ;; a nice theorem!
     (absento (var _.0)))
    ((cons '(_.0 _.1 . _.2) x)
     (=/= ((_.0 var)) ((_.1 var)) ((_.2 var)))
     (sym _.0 _.1 _.2))))

(test "mko fail backwards-2"
  (run 8 (e)
    (mko `(run* (q) ,e) '()))
  '(((== '_.0 '_.1)
     (=/= ((_.0 _.1)) ((_.0 var)) ((_.1 var)))
     (sym _.0 _.1))
    ((== '_.0 '())
     (=/= ((_.0 var)))
     (sym _.0))
    ((== '() '_.0)
     (=/= ((_.0 var)))
     (sym _.0))
    ((fresh (_.0) (== '_.1 '_.2))
     (=/= ((_.1 _.2)) ((_.1 var)) ((_.2 var)))
     (sym _.0 _.1 _.2))
    ((fresh (_.0) (== '_.1 '()))
     (=/= ((_.1 var)))
     (sym _.0 _.1))
    ((== '_.0 '(_.1 . _.2))
     (=/= ((_.0 var)))
     (sym _.0)
     (absento (var _.1) (var _.2)))
    ((fresh (_.0) (== '() '_.1))
     (=/= ((_.1 var)))
     (sym _.0 _.1))
    ((== '(_.0 . _.1) '_.2)
     (=/= ((_.2 var)))
     (sym _.2)
     (absento (var _.0) (var _.1)))))


(test "mko-2"
  (run* (q) (mko '(run* (x)
                    (conde
                      ((== x 'cat))
                      ((== 'dog x))))
                 q))
  '((cat dog)))

(test "mko-3"
  (run* (q) (mko '(run* (x)
                    (== 'cat 'cat))
                 q))
  '(((var z))))

(test "mko-4"
  (run* (q) (mko '(run* (x)
                    (fresh (y)
                      (== 'cat 'cat)))
                 q))
  '(((var z))))

(test "mko-5"
  (run* (q) (mko '(run* (x)
                    (fresh (y)
                      (== x 'cat)))
                 q))
  '((cat)))

(test "mko-6"
  (run* (q) (mko '(run* (x)
                    (fresh (x)
                      (== x 'cat)))
                 q))
  '(((var z))))

(test "mko-7"
  (run* (q) (mko '(run* (x)
                    (fresh (y)
                      (== y 'cat)))
                 q))
  '(((var z))))

(test "mko-8"
  (run* (q) (mko '(run* (x)
                    (fresh (y)
                      (== y 'cat)
                      (== x y)))
                 q))
  '((cat)))

(test "mko-9"
  (run* (q) (mko '(run* (x)
                    (fresh (y)
                      (== x y)
                      (== y 'cat)))
                 q))
  '((cat)))

(test "mko-10"
  (run* (q) (mko '(run* (x)
                    (fresh (y)
                      (fresh (z)
                        (== (cons y z) x)
                        (== z 'cat))
                      (== y 'dog)))
                 q))
  '(((dog . cat))))

(test "mko-11a"
  (run* (q) (mko '(run* (x)
                    (== '() x))
                 q))
  '((())))

(test "mko-11"
  (run* (q) (mko '(run* (x)
                    (fresh (y)
                      (fresh (z)
                        (== (cons y (cons z '())) x)
                        (== z 'cat))
                      (== y 'dog)))
                 q))
  '(((dog cat))))

(test "mko-12"
  (run* (q) (mko '(run* (x) (== '(cat) x)) q))
  '(((cat))))

(test "mko-13"
  (run* (q) (mko '(run* (x) (== '(cat dog fish) x)) q))
  '(((cat dog fish))))

(test "mko-14"
  (run* (q) (mko '(run* (x) (== '(cat dog . fish) x)) q))
  '(((cat dog . fish))))

(test "mko-15"
  (run* (q) (mko '(run* (x) (== '(cat dog . fish) (cons x '(dog . fish)))) q))
  '((cat)))

(test "mko-16"
  (run* (q) (mko '(run* (x) (== (cons x '(dog . fish)) '(cat dog . fish))) q))
  '((cat)))


(test "mko unify x with itself"
  (run* (q) (mko '(run* (x) (== x x)) q))
  '(((var z))))

(test "mko unify x with (cons y y)"
  (run* (q)
    (mko '(run* (x)
            (fresh (y)
              (== (cons y y) x)))
         q))
  '((((var (s z)) var (s z)))))

(test "mko occur-check-1"
  (run* (q) (mko '(run* (x) (== (cons x x) x)) q))
  '(()))

(test "mko occur-check-2"
  (run* (q) (mko '(run* (x) (== x (cons x x))) q))
  '(()))

(test "mko occur-check-3"
  (run* (q)
    (mko '(run* (x)
            (fresh (y)
              (== (cons y y) x)
              (== x y)))
         q))
  '(()))

(test "mko occur-check-4"
  (run* (q)
    (mko '(run* (x)
            (fresh (y)
              (== x y)
              (== (cons y y) x)))
         q))
  '(()))

(test "mko backwards-1"
  (run 10 (e) (mko e '(cat)))
  '(((run 1 (_.0) (== 'cat _.0))
     (sym _.0)) ((run 1 (_.0) (== _.0 'cat)) (sym _.0))
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
       (fresh (_.1)
         (== '_.2 '_.2)
         (fresh (_.3)
           (== 'cat _.3))))
     (=/= ((_.2 var)))
     (sym _.0 _.1 _.2 _.3))
    ((run 1 (_.0)
       (fresh (_.1)
         (fresh (_.2) (== '_.3 '_.3))
         (fresh (_.4) (== 'cat _.4))))
     (=/= ((_.3 var)))
     (sym _.0 _.1 _.2 _.3 _.4))
    ((run 1 (_.0)
       (fresh (_.1)
         (== '_.2 '_.2)
         (fresh (_.3) (== _.3 'cat))))
     (=/= ((_.2 var)))
     (sym _.0 _.1 _.2 _.3))
    ((run 1 (_.0)
       (fresh (_.1) (== 'cat _.0)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "mko backwards-2"
  (run 1 (e)
    (mko e '(cat))
    (mko e '(dog)))
  '(((run 1 (_.0)
       (conde
         ((== 'cat _.0))
         ((== 'dog _.0))))
     (sym _.0))))


(test "eval-mko 0"
  (run 1 (subst^)
    (fresh (c)
      (eval-mko '(== 'cat 'cat) '() 'z c '() subst^)))
  '(()))

(test "eval-mko 1"
  (run 1 (expr subst^)
    (fresh (c)
      (eval-mko expr '() 'z c '() subst^)))
  '((((== (quote _.0) (quote _.1)) #f)
     (=/= ((_.0 _.1)) ((_.0 var)) ((_.1 var)))
     (sym _.0 _.1))))

(test "eval-mko 2"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(== 'cat 'cat) '() 'z c '() subst^)))
  '(()))

(test "eval-mko 3"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(== 'cat 'dog) '() 'z c '() subst^)))
  '(#f))

(test "eval-mko 4"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(fresh (x)
                   (== x 'cat)
                   (== 'dog 'dog))
                '() 'z c '() subst^)))
  '((((var z) . cat))))

(test "eval-mko 5"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(fresh (x)
                   (== x 'cat)
                   (== 'cat x))
                '() 'z c '() subst^)))
  '((((var z) . cat))))

(test "eval-mko 6"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(fresh (x)
                   (== x 'cat) (== 'dog x))
                '() 'z c '() subst^)))
  '(#f))

(test "eval-mko 7"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(fresh (x)
                   (conde
                     ((== x 'cat))
                     ((== x 'dog))))
                '() 'z c '() subst^)))
  '((((var z) . cat)) (((var z) . dog))))

(test "eval-mko 8"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(fresh (x)
                   (conde
                     ((== 'cat 'cat))
                     ((== x 'dog))))
                '() 'z c '() subst^)))
  '(() (((var z) . dog))))

(test "eval-mko 9"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(conde
                   ((== 'cat 'cat))
                   ((== 'dog 'dog)))
                '() 'z c '() subst^)))
  '(() ()))

(test "eval-mko 10"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(fresh (x)
                   (conde
                     ((fresh (y)
                        (== y 'cat)
                        (== 'fish y)))
                     ((== x 'dog))))
                '() 'z c '() subst^)))
  '((((var z) . dog)) #f))

(test "eval-mko 11"
  (run* (subst^)
    (fresh (c)
      (eval-mko '(fresh (x)
                   (conde
                     ((== x 'dog))
                     ((fresh (y)
                        (== 'fish y)
                        (== y 'cat)))))
                '() 'z c '() subst^)))
  '((((var z) . dog)) #f))

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
  '(((fresh (_.0) (== 'dog _.0))
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
       (conde
         ((== 'dog _.0))
         (_.1)))
     (sym _.0))
    ((conde
       ((fresh (_.0)
          (== 'dog _.0)))
       (_.1))
     (sym _.0))
    ((conde
       (_.0)
       ((fresh (_.1) (== 'dog _.1))))
     (sym _.1))
    ((fresh (_.0)
       (conde
         (_.1)
         ((== 'dog _.0))))
     (sym _.0))))


(test "walko the dog"
  (run* (q) (walko 'dog '(((var z) . cat)) q))
  '(dog))

(test "walko the dogs"
  (run* (q) (walko '(dog . dog) '(((var z) . cat)) q))
  '((dog . dog)))

(test "walko the vars"
  (run* (q) (walko '((var z) . (var z)) '(((var z) . cat)) q))
  '(((var z) var z)))

(test "walko 1"
  (run* (q) (walko '(var z) '(((var z) . cat)) q))
  '(cat))

(test "walko 2"
  (run* (q) (walko '(var (s (s (s z)))) '(((var (s (s (s z)))) . cat)) q))
  '(cat))

(test "walko 3"
  (run* (q) (walko '(var (s (s (s z)))) '(((var (s (s z))) . cat)) q))
  '((var (s (s (s z))))))



(test "walk*o the vars"
  (run* (q) (walk*o '((var z) . (var z)) '(((var z) . cat)) q))
  '((cat . cat)))



(test "unifyo 1"
  (run* (subst^)
    (unifyo 'cat 'cat '() subst^))
  '(()))

(test "unifyo 2"
  (run* (subst^)
    (unifyo 'cat 'dog '() subst^))
  '(#f))

(test "unifyo 3"
  (run* (subst^)
    (unifyo '(var z) '((var (s z))) '() subst^))
  '((((var z) (var (s z))))))

(test "unifyo 4"
  (run* (subst^)
    (unifyo '(var z) '((var z)) '() subst^))
  '(#f))

(test "unifyo 5"
  (run* (subst^)
    (unifyo '(var z) '(var z) '() subst^))
  '(()))

(test "unifyo 6"
  (run* (subst^)
    (unifyo '(var z) '(var (s z)) '() subst^))
  '((((var z) . (var (s z))))))

(test "unifyo 7"
  (run* (subst^)
    (unifyo '(var z) '((var z)) '() subst^))
  '(#f))

(test "unifyo 8"
  (run* (subst^)
    (unifyo '((var z)) '(var z) '() subst^))
  '(#f))
