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
  (evalo/proper-or-improper-list e '(( (((dog)))) (rat))))


(run 10 (e v)
  (evalo/proper-or-improper-list-animals e v))

(run 10 (e v)
  (evalo/proper-list-animals e v))

(run 10 (e v)
  (evalo/flat-proper-list-animals e v))

(run 10 (e v)
  (evalo/flat-proper-list-distinct-animals e animals v))

(run 10 (e v)
  (evalo/deep-proper-list-distinct-animals e animals animals v))

(run 10 (e v)
  (evalo/deep-proper-non-empty-list-distinct-animals e animals animals v))

(run 10 (e v)
  (fresh (animals^)
    (evalo/deep-proper-non-empty-list-deep-distinct-animals e animals animals^ v)))
