(load "core-mk-implicit-unification.scm")
(load "../../faster-miniKanren/test-check.scm")

(test "mko-1"
  (run* (q) (mko '(run 1 (x)
                    (== x 'cat))
                 q))
  '(cat))

(test "mko-1b"
  (run* (e)
    (mko `(run 1 (x)
            (== ',e x))
         'cat))
  '(cat))

;;; hmmm--because the logic variables are represented as host logic
;;; variables, this test returns a second answer---*anything goes*.
;;; This second answer is ruled out in the implementations with tagged
;;; representations of variables.  Probably we do want to rule out the
;;; second answer, if we want to use the interpreter for program
;;; synthesis.  Otherwise, miniKanren can just synthesize programs
;;; that never extend the substitution.
;;
;; run 3 appears to diverge
(test "mko-1c"
  (run 2 (e)
    (mko `(run 1 (x)
            (== ,e x))
         'cat))
  '('cat
    x))

(test "mko-1c2"
  (run* (v)
    (mko `(run 1 (x)
            (== x x))
         v))
  '(_.0))

(test "mko-1c3"
  (run* (e)
    (mko `(run 1 (x)
            (== ',e x))
         'cat))
  '(cat))

(test "mko-1c4"
  (run 1 (e)
    (mko `(run 1 (x)
            ,e)
         'cat))
  '((== '_.0 '_.0)))
