(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(define evalo/proper-or-improper-list
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (conde
         ((== '() val))
         ((animal-symbolo val))))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/proper-or-improper-list e1 v1)
         (evalo/proper-or-improper-list e2 v2))))))

(define evalo/proper-list
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/proper-or-improper-list e1 v1)
         (evalo/proper-list e2 v2))))))

(define evalo/flat-proper-list
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(quote ,v1) e1)
         (animal-symbolo v1)
         (evalo/flat-proper-list e2 v2))))))

(define animal-symbolo
  (lambda (sym)
    (membero sym
             '(cat
               dog
               fox
               pig
               rat))))

(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        ((== y x))
        ((=/= y x)
         (membero x rest))))))
