(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "../faster-miniKanren/test-check.scm")
(load "core-mk-simple.scm")
(load "core-mk-complex.scm")

;; mko-simple tests
(test "mko-simple backwards-2"
  (run 1 (e)
    (mko-simple e 'cat)
    (mko-simple e 'dog))
  '(((run 1 (_.0)
       (conde
         ((== 'cat _.0))
         ((== 'dog _.0))))
     (sym _.0))))

(test "mko-simple forwards unclosed 0"
  (run 1 (expr)
    (fresh (e)
      (== `(run 1 (x)
             (conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  ((== 'fish x))))))
          expr)
      (mko-simple expr 'cat)
      (mko-simple expr 'dog)))
  '((run 1 (x)
      (conde
        ((== 'cat x))
        ((conde ((== 'dog x)) ((== 'fish x))))))))

(test "mko-simple forwards unclosed 1"
  (run 1 (expr)
    (fresh (e)
      (== `(run 1 (x)
             (conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  (,e)))))
          expr)
      (mko-simple expr 'cat)
      (mko-simple expr 'dog)))
  '((run 1 (x)
      (conde
        ((== 'cat x))
        ((conde
           ((== 'dog x))
           (_.0)))))))


;; mko-complex tests
#|
;; too slow to run!
(test "mko-complex backwards-2"
  (run 1 (e) (mko-complex e '(cat dog)))
  '???)
|#

(test "mko-complex forwards unclosed 0a"
  (run* (expr)
    (fresh (e)
      (== `(run* (x)
             (conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  ((== 'fish x))))))
          expr)
      (mko-complex expr '(cat dog fish))))
  '((run* (x)
      (conde
        ((== (quote cat) x))
        ((conde
           ((== (quote dog) x))
           ((== (quote fish) x))))))))

(test "mko-complex forwards unclosed 0b"
  (run* (expr)
    (fresh (e)
      (== `(run* (x)
             (conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  ((== 'fish x))))))
          expr)
      (mko-complex expr '(cat dog))))
  '())

(test "mko-complex forwards unclosed 1a"
  (run 1 (expr)
    (fresh (e)
      (== `(run* (x)
             (conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  (,e)))))
          expr)
      (mko-complex expr '(cat dog fish))))
  '((run* (x)
      (conde
        ((== 'cat x))
        ((conde
           ((== 'dog x))
           ((== 'fish x))))))))

(test "mko-complex forwards unclosed 1b"
  (run 1 (expr)
    (fresh (e)
      (== `(run* (x)
             (conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  (,e)))))
          expr)
      (mko-complex expr '(cat dog))))
  '(((run* (x)
       (conde
         ((== 'cat x))
         ((conde
            ((== 'dog x))
            ((== '_.0 '_.1))))))
   (=/= ((_.0 _.1)) ((_.0 var)) ((_.1 var)))
   (sym _.0 _.1))))



;; combined mko-simple and mko-complex closed tests
(test "mko-simple and mko-complex combined tests 1a"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (mko-simple simple-expr 'cat)
      (mko-simple simple-expr 'dog)
      (mko-complex complex-expr '(cat dog))))
  '((conde
      ((== 'cat x))
      ((== 'dog x)))))

;; thanks for the test, Nada!
(test "mko-simple and mko-complex combined tests 1b"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (mko-simple simple-expr 'dog)
      (mko-simple simple-expr 'cat)
      (mko-complex complex-expr '(cat dog))))
  '((conde
      ((== 'cat x))
      ((== 'dog x)))))

(test "mko-simple and mko-complex combined tests 1c"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (mko-simple simple-expr 'cat)
      (mko-simple simple-expr 'dog)
      (mko-complex complex-expr '(dog cat))))
  '((conde
      ((== 'dog x))
      ((== 'cat x)))))

(test "mko-simple and mko-complex combined tests 2"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (== '(conde
             ((== 'cat x))
             ((== 'dog x)))
          e)
      (mko-simple simple-expr 'cat)
      (mko-simple simple-expr 'dog)
      (mko-complex complex-expr '(cat dog))))
   '((conde
       ((== (quote cat) x))
       ((== (quote dog) x)))))

(test "mko-simple and mko-complex combined tests 3"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (== '(conde
             ((== 'cat x))
             ((conde
                ((== 'dog x))
                ((== 'fish x)))))
          e)
      (mko-simple simple-expr 'cat)
      (mko-simple simple-expr 'dog)
      (mko-complex complex-expr '(cat dog))))
   '())

(test "mko-simple and mko-complex combined tests 4"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (== '(conde
             ((== 'cat x))
             ((conde
                ((== 'dog x))
                ((== 'fish 'bat)))))
          e)
      (mko-simple simple-expr 'cat)
      (mko-simple simple-expr 'dog)
      (mko-complex complex-expr '(cat dog))))
   '((conde
       ((== 'cat x))
       ((conde
          ((== 'dog x))
          ((== 'fish 'bat)))))))

(test "mko-simple and mko-complex combined tests 5"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (mko-simple simple-expr 'cat)
      (mko-simple simple-expr 'dog)
      (mko-simple simple-expr 'fish)
      (mko-complex complex-expr '(cat dog fish))))
  '((conde
      ((== 'cat x))
      ((conde
         ((== 'dog x))
         ((== 'fish x)))))))

(test "mko-simple and mko-complex combined tests 6a"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (fresh (e^)
        (== `(conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  (,e^))))
            e))
      (mko-simple simple-expr 'cat)
      (mko-simple simple-expr 'dog)
      (mko-complex complex-expr '(cat dog))))
  '(((conde
       ((== 'cat x))
       ((conde
          ((== 'dog x))
          ((== '_.0 '_.1)))))
     (=/= ((_.0 _.1)) ((_.0 var)) ((_.1 var)))
     (sym _.0 _.1))))

(test "mko-simple and mko-complex combined tests 6b"
  (run 1 (e)
    (fresh (simple-expr complex-expr)
      (== `(run 1 (x) ,e) simple-expr)
      (== `(run* (x) ,e) complex-expr)
      (fresh (e^)
        (== `(conde
               ((== 'cat x))
               ((conde
                  ((== 'dog x))
                  (,e^))))
            e))
      (mko-simple simple-expr 'dog)
      (mko-simple simple-expr 'cat)
      (mko-complex complex-expr '(cat dog))))
  '(((conde
       ((== 'cat x))
       ((conde
          ((== 'dog x))
          ((== '_.0 '_.1)))))
     (=/= ((_.0 _.1)) ((_.0 var)) ((_.1 var)))
     (sym _.0 _.1))))
