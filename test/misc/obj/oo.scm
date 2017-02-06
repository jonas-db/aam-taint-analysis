(define global 1)

(define person (object))
(define x person)

(define (print message) (display message) (newline))


(define-data-property person "_name" "james")

(define-accessor-property person "name"
	(lambda () (begin (print "getter called") (get-property person "_name")))
	(lambda (v) (begin (print "setter called") (display v) (set-property person "_name" v))))

;(display global)

(print "get name")
(define n (get-property person "name")) ; -> "james"
(print n)

(print "set name")
(set-property person "name" "jonas2") ; -> "jonas", global = 2

(print "get name")
(print (get-property person "name")) ; -> "jonas"

(set-property person "name" 2) ; -> 2, global = 3

(get-property x "_name")
