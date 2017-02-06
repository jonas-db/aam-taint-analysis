; A simple handler closure
; The idea is that of "package-manager" which handles incoming packets using the "handler" closure
; In this case, the handler will simply hold a counter indicating the amount of packets that were handled
; Procudes the following output:
; "handling packet..."
; "_login"
; ".. succesfully handled packet #1"
; "handling packet..."
; "_logout"
; ".. succesfully handled packet #2"
; "total amount of packet = 2"

(define (print message) (display message) (newline))

; The handler closure (holding state by means of a counter)
(define (handler)
  (let ((counter 0))    
    (lambda (m)
      (if (eq? m 'get)
          counter
          (begin (set! counter (+ counter 1)))))))

; The handle function of the packet-manager          
(define handle
	(lambda (packet) 
		(begin 
			(display "handling packet...")
			(print (get-property packet "cmd"))
					
			; Increment counter
			((get-property packet-manager "handler") 'inc)
					
			(display ".. succesfully handled packet #")
			(display ((get-property packet-manager "handler") 'get)))))

; The package manager object
(define packet-manager (object))
; The handler
(define-data-property packet-manager "handler" (handler)) 
; The handle 
(define-data-property packet-manager "handle" handle)

; A login packet
(define login-packet (object))
(define-data-property login-packet "cmd" "_login")
(define-data-property login-packet "data" "jonas")

; A logout packet
(define logout-packet (object))
(define-data-property logout-packet "cmd" "_logout")
(define-data-property logout-packet "data" "jonas")

; Send login and logout packet
((get-property packet-manager "handle") login-packet)
((get-property packet-manager "handle") logout-packet)

(display "total amount of packet = ")
(print ((get-property packet-manager "handler") 'get))