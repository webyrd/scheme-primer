(load "cons.scm")

(run 10 (e v)
  (evalo/proper-or-improper-list e v))

(run 10 (e v)
  (evalo/proper-list e v))

(run 10 (e v)
  (evalo/flat-proper-list e v))

(run 10 (e v)
  (evalo/flat-proper-list-distinct-animals e animals v))

(run 10 (e v)
  (evalo/deep-proper-list-distinct-animals e animals animals v))

(run 10 (e v)
  (evalo/deep-proper-non-empty-list-distinct-animals e animals animals v))

(run 10 (e v)
  (fresh (animals^)
    (evalo/deep-proper-non-empty-list-deep-distinct-animals e animals animals^ v)))
