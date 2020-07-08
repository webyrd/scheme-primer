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

(define exit-primer
  (lambda (menu-item-name)
    (display "exiting Scheme Primer")
    (newline)))

(define unimplemented-menu-item
  (lambda (menu-item-name)
    (display "Sorry--this menu item, \"")
    (display menu-item-name)
    (display "\", has not been implemented yet.")
    (newline)
    (display "Please choose a different menu item.")
    (newline)
    (newline)        
    (main-menu)))

(define main-menu-items
  `(("exit Scheme Primer" . ,exit-primer)
    ("parentheses practice" . ,unimplemented-menu-item)
    ("'cons' practice" . ,unimplemented-menu-item)
    ("'car' and 'cdr' practice" . ,unimplemented-menu-item)
    ("box-and-pointers practice" . ,unimplemented-menu-item)
    ("conditionals practice" . ,unimplemented-menu-item)
    ("'lambda' and application practice" . ,unimplemented-menu-item)
    ("variables, scope, binding, and shadowing practice" . ,unimplemented-menu-item)
    ("point-wise programming practice" . ,unimplemented-menu-item)
    ("simple recursion practice" . ,unimplemented-menu-item)
    ("'quasiquote', 'unquote', and 'unquote-splicing' practice" . ,unimplemented-menu-item)
    ("pattern-matching practice" . ,unimplemented-menu-item)
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
    (for-each (lambda (pr i)
                (display "(")
                (display i)
                (display ") ")
                (display (car pr))
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
         (let ((pr (list-ref main-menu-items choice)))
           (let ((menu-item-name (car pr))
                 (handle-menu-item-procedure (cdr pr)))
             (newline)
             (display "You chose (")
             (display choice)
             (display "), ")
             (display menu-item-name)
             (display ".")
             (newline)
             (newline)
             (display "Come on!  Here we go!")
             (newline)
             (newline)
             (handle-menu-item-procedure menu-item-name))))
        (else
         (newline)
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
