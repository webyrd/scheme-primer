(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(define evalo/proper-or-improper-list
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (conde
         ((== '() val))
         ((symbolo val))))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/proper-or-improper-list e1 v1)
         (evalo/proper-or-improper-list e2 v2))))))

(define evalo/proper-or-improper-list-cons-count-animals
  (lambda (expr cons-count cons-count^ val)
    (conde
      ((== `(quote ,val) expr)
       (== cons-count cons-count^)
       (conde
         ((== '() val))
         ((animal-symbolo val))))
      ((fresh (e1 e2 v1 v2 cons-count-1 cons-count^^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(s ,cons-count-1) cons-count)
         (evalo/proper-or-improper-list-cons-count-animals e1 cons-count-1 cons-count^^ v1)
         (evalo/proper-or-improper-list-cons-count-animals e2 cons-count^^ cons-count^ v2))))))

(define evalo/proper-or-improper-list-animals
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (conde
         ((== '() val))
         ((animal-symbolo val))))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/proper-or-improper-list-animals e1 v1)
         (evalo/proper-or-improper-list-animals e2 v2))))))

(define evalo/proper-list-animals
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/proper-or-improper-list-animals e1 v1)
         (evalo/proper-list-animals e2 v2))))))

(define evalo/flat-proper-list-animals
  (lambda (expr val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(quote ,v1) e1)
         (animal-symbolo v1)
         (evalo/flat-proper-list-animals e2 v2))))))

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

(define evalo/deep-proper-list-distinct-animals
  (lambda (expr all-animals animals-at-this-level val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2 animals-at-this-level^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (conde
           ((== `(quote ,v1) e1)
            (remove-exactly-oneo v1 animals-at-this-level animals-at-this-level^))
           ((== animals-at-this-level animals-at-this-level^)
            (evalo/deep-proper-list-distinct-animals e1 all-animals all-animals v1)))
         (evalo/deep-proper-list-distinct-animals e2 all-animals animals-at-this-level^ v2))))))

(define evalo/deep-proper-non-empty-list-distinct-animals
  (lambda (expr all-animals animals-at-this-level val)
    (conde
      ((fresh (v animals-at-this-level^)
         (== `(cons (quote ,v) (quote ())) expr)
         (== `(,v . ()) val)
         (remove-exactly-oneo v animals-at-this-level animals-at-this-level^)))
      ((fresh (e1 e2 v1 v2 animals-at-this-level^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (conde
           ((== `(quote ,v1) e1)
            (remove-exactly-oneo v1 animals-at-this-level animals-at-this-level^))
           ((== animals-at-this-level animals-at-this-level^)
            (evalo/deep-proper-non-empty-list-distinct-animals e1 all-animals all-animals v1)))
         (evalo/deep-proper-non-empty-list-distinct-animals e2 all-animals animals-at-this-level^ v2))))))

(define evalo/deep-proper-non-empty-list-deep-distinct-animals
  (lambda (expr animals animals^ val)
    (conde
      ((fresh (v)
         (== `(cons (quote ,v) (quote ())) expr)
         (== `(,v . ()) val)
         (remove-exactly-oneo v animals animals^)))
      ((fresh (e1 e2 v1 v2 animals^^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (conde
           ((== `(quote ,v1) e1)
            (remove-exactly-oneo v1 animals animals^^))
           ((evalo/deep-proper-non-empty-list-deep-distinct-animals e1 animals animals^^ v1)))
         (evalo/deep-proper-non-empty-list-deep-distinct-animals e2 animals^^ animals^ v2))))))

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
