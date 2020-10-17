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

(define evalo/proper-or-improper-list-cons-count-symbols
  (lambda (expr symbols cons-count cons-count^ val)
    (conde
      ((== `(quote ,val) expr)
       (== cons-count cons-count^)
       (conde
         ((== '() val))
         ((membero val symbols))))
      ((fresh (e1 e2 v1 v2 cons-count-1 cons-count^^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(s ,cons-count-1) cons-count)
         (evalo/proper-or-improper-list-cons-count-symbols e1 symbols cons-count-1 cons-count^^ v1)
         (evalo/proper-or-improper-list-cons-count-symbols e2 symbols cons-count^^ cons-count^ v2))))))

(define evalo/proper-or-improper-list-symbols
  (lambda (expr symbols val)
    (conde
      ((== `(quote ,val) expr)
       (conde
         ((== '() val))
         ((membero val symbols))))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/proper-or-improper-list-symbols e1 symbols v1)
         (evalo/proper-or-improper-list-symbols e2 symbols v2))))))

(define evalo/proper-list-symbols
  (lambda (expr symbols val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (evalo/proper-or-improper-list-symbols e1 symbols v1)
         (evalo/proper-list-symbols e2 symbols v2))))))

(define evalo/flat-proper-list-symbols
  (lambda (expr symbols val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(quote ,v1) e1)
         (membero v1 symbols)
         (evalo/flat-proper-list-symbols e2 symbols v2))))))

(define evalo/flat-proper-list-distinct-symbols
  (lambda (expr symbols val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2 symbols^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(quote ,v1) e1)
         (remove-exactly-oneo v1 symbols symbols^)
         (evalo/flat-proper-list-distinct-symbols e2 symbols^ v2))))))

(define evalo/deep-proper-list-distinct-symbols
  (lambda (expr all-symbols symbols-at-this-level val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val))
      ((fresh (e1 e2 v1 v2 symbols-at-this-level^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (conde
           ((== `(quote ,v1) e1)
            (remove-exactly-oneo v1 symbols-at-this-level symbols-at-this-level^))
           ((== symbols-at-this-level symbols-at-this-level^)
            (evalo/deep-proper-list-distinct-symbols e1 all-symbols all-symbols v1)))
         (evalo/deep-proper-list-distinct-symbols e2 all-symbols symbols-at-this-level^ v2))))))

(define evalo/deep-proper-non-empty-list-distinct-symbols
  (lambda (expr all-symbols symbols-at-this-level val)
    (conde
      ((fresh (v symbols-at-this-level^)
         (== `(cons (quote ,v) (quote ())) expr)
         (== `(,v . ()) val)
         (remove-exactly-oneo v symbols-at-this-level symbols-at-this-level^)))
      ((fresh (e1 e2 v1 v2 symbols-at-this-level^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (conde
           ((== `(quote ,v1) e1)
            (remove-exactly-oneo v1 symbols-at-this-level symbols-at-this-level^))
           ((== symbols-at-this-level symbols-at-this-level^)
            (evalo/deep-proper-non-empty-list-distinct-symbols e1 all-symbols all-symbols v1)))
         (evalo/deep-proper-non-empty-list-distinct-symbols e2 all-symbols symbols-at-this-level^ v2))))))

(define evalo/deep-proper-non-empty-list-deep-distinct-symbols
  (lambda (expr symbols symbols^ val)
    (conde
      ((fresh (v)
         (== `(cons (quote ,v) (quote ())) expr)
         (== `(,v . ()) val)
         (remove-exactly-oneo v symbols symbols^)))
      ((fresh (e1 e2 v1 v2 symbols^^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (conde
           ((== `(quote ,v1) e1)
            (remove-exactly-oneo v1 symbols symbols^^))
           ((evalo/deep-proper-non-empty-list-deep-distinct-symbols e1 symbols symbols^^ v1)))
         (evalo/deep-proper-non-empty-list-deep-distinct-symbols e2 symbols^^ symbols^ v2))))))

(define evalo/deep-proper-non-empty-list-deep-distinct-symbols-count
  (lambda (expr symbols symbols^ cons-count cons-count^ val)
    (conde
      ((fresh (v)
         (== `(cons (quote ,v) (quote ())) expr)
         (== `(s ,cons-count^) cons-count)
         (== `(,v . ()) val)
         (remove-exactly-oneo v symbols symbols^)))
      ((fresh (e1 e2 v1 v2 symbols^^ cons-count-1 cons-count^^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(s ,cons-count-1) cons-count)
         (conde
           ((== `(quote ,v1) e1)
            (== cons-count^^ cons-count-1)
            (remove-exactly-oneo v1 symbols symbols^^))
           ((evalo/deep-proper-non-empty-list-deep-distinct-symbols-count e1 symbols symbols^^ cons-count-1 cons-count^^ v1)))
         (evalo/deep-proper-non-empty-list-deep-distinct-symbols-count e2 symbols^^ symbols^ cons-count^^ cons-count^ v2))))))

(define evalo/deep-proper-list-deep-distinct-symbols-count
  (lambda (expr symbols symbols^ cons-count cons-count^ val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val)
       (== symbols symbols^)
       (== cons-count cons-count^))
      ((fresh (e1 e2 v1 v2 symbols^^ cons-count-1 cons-count^^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (== `(s ,cons-count-1) cons-count)
         (conde
           ((== `(quote ,v1) e1)
            (== cons-count^^ cons-count-1)
            (remove-exactly-oneo v1 symbols symbols^^))
           ((evalo/deep-proper-list-deep-distinct-symbols-count e1 symbols symbols^^ cons-count-1 cons-count^^ v1)))
         (evalo/deep-proper-list-deep-distinct-symbols-count e2 symbols^^ symbols^ cons-count^^ cons-count^ v2))))))

(define evalo/deep-proper-list-deep-distinct-symbols
  (lambda (expr symbols symbols^ val)
    (conde
      ((== `(quote ,val) expr)
       (== '() val)
       (== symbols symbols^))
      ((fresh (e1 e2 v1 v2 symbols^^)
         (== `(cons ,e1 ,e2) expr)
         (== `(,v1 . ,v2) val)
         (conde
           ((== `(quote ,v1) e1)
            (remove-exactly-oneo v1 symbols symbols^^))
           ((evalo/deep-proper-list-deep-distinct-symbols e1 symbols symbols^^ v1)))
         (evalo/deep-proper-list-deep-distinct-symbols e2 symbols^^ symbols^ v2))))))


(define animals
  '(cat
    dog
    fox
    pig
    rat
    ant
    bat))

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
