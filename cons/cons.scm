(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(define evalo/cons
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (conde
         ((== '() val))
         ((animal-symbolo val))))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/cons e1 v1)
         (evalo/cons e2 v2))))))

(define animal-symbolo
  (lambda (sym)
    (membero sym
             '(cat
               dog
               fox
               pig))))

(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        ((== y x))
        ((=/= y x)
         (membero x rest))))))
