(define (print message) (display message) (newline))

(define packet-manager (object))
(define-data-property packet-manager "session" (object))
(define-data-property packet-manager "handle" 
	(lambda (packet) ( (get-property packet-manager (get-property packet "cmd")) (get-property packet "data")) ))
	
(define-data-property packet-manager "_login" 
	(lambda (v) (begin (print "someone has logged in") (set-property packet-manager "session" #t)) ))
	
(define-data-property packet-manager "_logout" 
	(lambda (v) (if (get-property (get-property packet-manager "session") v) ; DOES NOT WORK, ONLY STRINGS A PROPERTY 
						(begin (set-property packet-manager "session" v #f) (print "someone has logged out"))
						(print "cannot log out, since you are not logged in")) ))

(define login-packet (object))
(define-data-property login-packet "cmd" "_login")
(define-data-property login-packet "data" "jonas")

(define logout-packet (object))
(define-data-property logout-packet "cmd" "_logout")
(define-data-property logout-packet "data" "jonas")

((get-property packet-manager "handle") logout-packet) ; you cannot logout if you are not logged in

((get-property packet-manager "handle") login-packet) ; login

((get-property packet-manager "handle") logout-packet) ; ok, logout is possible now