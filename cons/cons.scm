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

(define evalo/flat-proper-list-distinct-animals
  (lambda (expr animals val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2 animals^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(quote ,v1) e1)
         (remove-exactly-oneo v1 animals animals^)
         (evalo/flat-proper-list-distinct-animals e2 animals^ v2))))))

(define animals
  '(cat
    dog
    fox
    pig
    rat
    ant
    bat))

(define animal-symbolo
  (lambda (sym)
    (membero sym animals)))

(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        ((== y x))
        ((=/= y x)
         (membero x rest))))))

(define remove-exactly-oneo
  (lambda (x ls ls-x)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        ((== y x)
         (== rest ls-x))
        ((=/= y x)
         (remove-exactly-oneo x rest ls-x))))))

(define removeo
  (lambda (x ls ls-x)
    (conde
      ((== '() ls) (== '() ls-x))
      ((fresh (y rest)
         (== `(,y . ,rest) ls)
         (conde
           ((== y x)
            (== rest ls-x))
           ((=/= y x)
            (removeo x rest ls-x))))))))
