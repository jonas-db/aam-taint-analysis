; fig 1: http://researcher.ibm.com/researcher/files/us-otripp/pldi09.pdf
; TAJ: Effective Taint Analysis of Web Applications
; zerocfa: s2 is must tainted -> false positive, is "maybe" because fname and lname are bound to x 
; onecfa s1 is must tainted -> true positive

(define (get-listener event)
	(letrec ((t1 (get-property event 'fname))
			 (t2 (get-property event 'lname))
			 (map (object)))
			 	(set-property map 'fname t1)
			 	(set-property map 'lname t2)
			 	(set-property map 'date (object))
				(letrec ((s1 (id (get-property map 'fname)))
						 (s2 (id (sanitize (get-property map 'lname))))
						 (s3 (get-property map 'date)))
						  (sink s3)
						  (sink s2)
						  (sink s1))))

(define (id x) x)

(letrec ((evt (event 'get))
		 (servlet (object)))
	(define-data-property evt 'fname (taint 'jonas))
	(define-data-property evt 'lname (taint 'EVIL))
	(add-event-listener servlet 'get get-listener)
	(emit servlet evt)
	(event-queue))

