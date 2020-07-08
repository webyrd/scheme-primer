;; Scheme Primer

(define primer-version-string "0.00000001")

(define print-greeting
  (lambda ()
    (display "Welcome to the Scheme Primer, version ")
    (display primer-version-string)
    (newline)
    (newline)
    (display "Scheme is a programming language, and is a dialect of the LISP family of languages.")
    (newline)
    (newline)
    (display "This interactive Scheme Primer will teach you Scheme, and challenge you to write Scheme code of increasing complexity.")
    (newline)
    (newline)
    (display "The Scheme Primer will automatically adjust the difficulty of the exercises depending on how well you have performed on previous exercises.")
    (newline)
    (newline)
    (display "The Scheme Primer is itself written in Scheme.  The Scheme Primer will teach you how to implement enough Scheme to run the Primer in your own Scheme implementation (\"Meta-circular Primer\"/\"Primer-ception\").")
    (newline)
    (newline)
    (display "The Scheme Primer also allows you to jump to specific lessons or pratice exercises.")
    (newline)
    (newline)))

(define main-menu-items
  '("parentheses practice"
    "'cons' practice"
    "'car' and 'cdr' practice"
    "box-and-pointers practice"
    "conditionals practice"
    "'lambda' and application practice"
    "variables, scope, binding, and shadowing practice"
    "point-wise programming practice"
    "simple recursion practice"
    "'quasiquote', 'unquote', and 'unquote-splicing' practice"
    "pattern-matching practice"
    ))

(define iota
  (lambda (n)
    (let loop ((i 0))
      (cond
        ((= i n) '())
        (else (cons i (loop (add1 i))))))))

(define main-menu
  (lambda ()
    (display "Main Menu")
    (newline)
    (display "----------")
    (newline)
    (for-each (lambda (str i)
                (display "(")
                (display i)
                (display ") ")
                (display str)
                (newline))
              main-menu-items
              (iota (length main-menu-items)))
    (display "----------")
    (newline)
    (display "Please enter the number of your choice from the menu above:")
    (newline)
    (let ((choice (read)))
      (cond
        ((and (number? choice)
              (integer? choice)
              (>= choice 0)
              (< choice (length main-menu-items)))
         (display choice))
        (else
         (display "Sorry--I didn't understand your choice!")
         (newline)         
         (display "Please try again!  Please enter a number between 0 and ")
         (display (sub1 (length main-menu-items)))
         (display ", inclusive.")
         (newline)
         (newline)
         (main-menu))))))

(print-greeting)

(main-menu)

;; self-evaluating literals

;; cons

;; car, cdr

;; quote

;; box and pointers

;; s-expressions

;; conditionals

;; definitions

;; variables, scope, binding, and shadowing

;; lambda and application

;; what does this expression evaluate to?

;; recursion

;; quasiquote and unquote

;; pattern-matching

;; interpreters

;; macros
