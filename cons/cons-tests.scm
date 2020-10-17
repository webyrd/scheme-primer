(load "cons.scm")

(run 10 (e v)
  (evalo/proper-or-improper-list e v))

(run* (e)
  (evalo/proper-or-improper-list e '()))

(run* (e)
  (evalo/proper-or-improper-list e '(cat)))

(run* (e)
  (evalo/proper-or-improper-list e '(dog rat)))

(run* (e)
  (evalo/proper-or-improper-list e '(cat (dog) . rat)))

(run* (e)
  (evalo/proper-or-improper-list e '(cat (((dog))) rat)))

(run* (e)
  (evalo/proper-or-improper-list e '(((cat) (((dog)))) (rat))))



(run* (e v)
  (evalo/proper-or-improper-list-cons-count-symbols e animals 'z 'z v))

(run* (e v)
  (evalo/proper-or-improper-list-cons-count-symbols e animals '(s z) 'z v))

(run* (e v)
  (evalo/proper-or-improper-list-cons-count-symbols e animals '(s (s z)) 'z v))


(run 10 (e v)
  (evalo/proper-or-improper-list-symbols e animals v))

(run 10 (e v)
  (evalo/proper-list-symbols e animals v))

(run 10 (e v)
  (evalo/flat-proper-list-symbols e animals v))

(run 10 (e v)
  (evalo/flat-proper-list-distinct-symbols e animals v))

(run 10 (e v)
  (evalo/deep-proper-list-distinct-symbols e animals animals v))

(run 10 (e v)
  (evalo/deep-proper-non-empty-list-distinct-symbols e animals animals v))

(run 10 (e v)
  (fresh (animals^)
    (evalo/deep-proper-non-empty-list-deep-distinct-symbols e animals animals^ v)))


(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-non-empty-list-deep-distinct-symbols-count e animals animals^ 'z 'z v)))

(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-non-empty-list-deep-distinct-symbols-count e animals animals^ '(s z) 'z v)))

(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-non-empty-list-deep-distinct-symbols-count e animals animals^ '(s (s z)) 'z v)))

(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-non-empty-list-deep-distinct-symbols-count e animals animals^ '(s (s (s z))) 'z v)))




(run 10 (e v)
  (fresh (animals^)
    (evalo/deep-proper-list-deep-distinct-symbols e animals animals^ v)))


(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ 'z 'z v)))

(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s z) 'z v)))

(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s (s z)) 'z v)))

(run* (e v)
  (fresh (animals^)
    (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s (s (s z))) 'z v)))

(length
 (run* (e v)
   (fresh (animals^)
     (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s (s (s z))) 'z v))))

(map cadr
     (run* (e v)
       (fresh (animals^)
         (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s (s (s z))) 'z v))))

(length
 (run* (e v)
   (fresh (animals^)
     (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s (s (s (s z)))) 'z v))))

(map cadr
     (run* (e v)
       (fresh (animals^)
         (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s (s (s (s z)))) 'z v))))

(andmap (lambda (e/v) (let ((e (car e/v)) (v (cadr e/v))) (equal? (eval e) v)))
        (run* (e v)
          (fresh (animals^)
            (evalo/deep-proper-list-deep-distinct-symbols-count e animals animals^ '(s (s (s (s z)))) 'z v))))
