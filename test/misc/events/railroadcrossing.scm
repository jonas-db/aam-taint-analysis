; Example of a railway crossing as in http://quebec.upb.de/download/papers/BNBM05_ESG_TestGen.pdf
; TODO: missing logic of clicking on signal when gate is closed for example

(define (print message) (display message) (newline))

(define STATE_GREEN 0)
(define STATE_RED 1)

(define STATE_OPEN 2)
(define STATE_CLOSED 3)

(define railway-crossing (object))
(define-data-property railway-crossing "signal" STATE_GREEN)
(define-data-property railway-crossing "gate" STATE_OPEN)
	
; button to toggle signal
(define signal-button (object))

; button to toggle gate
(define gate-button (object))

; button simulates a vehicle 
(define vehicle-button (object))

; button simulates a train passing 
(define train-button (object))

(define train_handler (lambda (evt) (dispatch-event railway-crossing "train" #f)))
(define vehicle_handler (lambda (evt) (dispatch-event railway-crossing "vehicle" #f)))
(define gate_handler (lambda (evt) (dispatch-event railway-crossing "toggle_gate" #f)))
(define signal_handler (lambda (evt) (dispatch-event railway-crossing "toggle_signal" #f)))

(add-event-listener railway-crossing "train" (lambda (evt) (print "train passing")))
(add-event-listener railway-crossing "vehicle" (lambda (evt) (print "vehicle passing")))

(add-event-listener railway-crossing "toggle_signal" 
	(lambda (evt) 
		(if (= (get-property railway-crossing "signal") STATE_GREEN)
			; when toggling to red, enable event listener of the gate
			(begin
				(add-event-listener gate-button "click" gate_handler)
				(set-property railway-crossing "signal" STATE_RED)
				(print "toggled signal to red"))
			; else we remove the event listener
			(begin
				(remove-event-listener gate-button "click" gate_handler)
				(set-property railway-crossing "signal" STATE_GREEN)
				(print "toggled signal to green")))))
				

(add-event-listener railway-crossing "toggle_gate" 
	(lambda (evt) 
		(if (= (get-property railway-crossing "gate") STATE_OPEN)
			; when toggling to closed, enable event listener of the train + disable vehicle
			(begin
				(add-event-listener train-button "click" train_handler)
				(remove-event-listener vehicle-button "click" vehicle_handler)
				(set-property railway-crossing "gate" STATE_CLOSED)
				(print "toggled gate to closed"))
			; else we remove the event listener
			(begin
				(remove-event-listener train-button "click" train_handler)
				(add-event-listener vehicle-button "click" vehicle_handler)
				(set-property railway-crossing "gate" STATE_OPEN)
				(print "toggled gate to open")))))

; window
(define window (object))
(add-event-listener window "load" (lambda (evt) 
	(begin 
		(add-event-listener signal-button "click"
			(lambda (evt) (dispatch-event railway-crossing "toggle_signal" #f)))
	)))

