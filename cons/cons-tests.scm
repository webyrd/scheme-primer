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

(run* (e v)
  (fresh (letters^)
    (evalo/deep-proper-list-deep-distinct-symbols-count e letters letters^ '(s (s (s z))) 'z v)))

(run* (e v)
  (fresh (dan-scheme^)
    (evalo/deep-proper-list-deep-distinct-symbols-count e dan-scheme dan-scheme^ '(s (s (s z))) 'z v)))

(run* (e v)
  (fresh (symbols symbols^)
    (membero symbols (list animals letters dan-scheme))
    (evalo/deep-proper-list-deep-distinct-symbols-count e symbols symbols^ '(s (s z)) 'z v)))

(map cadr
     (run* (e v)
       (fresh (symbols symbols^)
         (membero symbols (list animals letters dan-scheme))
         (evalo/deep-proper-list-deep-distinct-symbols-count e symbols symbols^ '(s (s z)) 'z v))))

(andmap
  (lambda (e/v)
    (let ((e (car e/v))
          (v (cadr e/v)))
      (and (equal? (eval e) v)
           (equal? (run* (q) (evalo/proper-or-improper-list q v)) (list e)))))
  (run* (e v)
    (fresh (symbols symbols^)
      (membero symbols (list animals letters dan-scheme))
      (evalo/deep-proper-list-deep-distinct-symbols-count e symbols symbols^ '(s (s z)) 'z v))))


(let ((e/v* (run* (e v)
              (fresh (symbols symbols^)
                (membero symbols (list animals letters dan-scheme))
                (evalo/deep-proper-list-deep-distinct-symbols-count e symbols symbols^ '(s (s z)) 'z v)))))
  (let ((e/v*-length (length e/v*)))
    (let ((num-problems 5)
          (start-time (current-time 'time-monotonic)))      
      (let loop ((i 1)
                 (pass 0)
                 (fail 0))
        (cond
          ((= i num-problems)
           (let ((end-time (current-time 'time-monotonic)))
             (let ((duration/seconds (time-second (time-difference end-time start-time))))
               (printf "\n-----------------------\n\n")
               (printf "finished!\n")
               (printf "elapsed time: ~s seconds\n" duration/seconds)
               (printf "passed: ~s of ~s" pass (+ pass fail))
               (when (not (zero? (+ pass fail)))
                 (printf "  (~s%)" (exact->inexact (* (/ pass (+ pass fail)) 100))))
               (newline))))
          (else
           (let ((e/v (list-ref e/v* (random e/v*-length))))
             (let ((e (car e/v))
                   (v (cadr e/v)))
               (printf "\n-----------------------\n\n")
               (printf "problem ~s of ~s\n\n" i num-problems)
               (printf "enter an expression containing only 'cons', 'quote', symbols, and the empty list ()\n")
               (printf "or enter 'exit'\n\n")
               (printf "~s\n\n" v)
               (let ((entered-e (read)))
                 (cond
                   ((or (equal? 'exit entered-e) (equal? '(quote exit) entered-e))
                    (loop num-problems
                          pass
                          fail))
                   (else
                    (newline)
                    (let-values (((pass fail) (if (equal? entered-e e)
                                                  (begin
                                                    (printf "correct!\n")
                                                    (values (add1 pass) fail))
                                                  (begin
                                                    (printf "incorrect!\n\n")
                                                    (printf "correct answer: ~a\n" e)
                                                    (values pass (add1 fail))))))
                      (newline)
                      (printf "passed: ~s of ~s\n" pass (+ pass fail))
                      (loop (add1 i)
                            pass
                            fail)))))))))))))
