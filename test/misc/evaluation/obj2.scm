(define foo (lambda (z) 
	(letrec ((x (get-property z 'g))
			 (w (taint 1)))
				 (set-property x 'f w))))

(define (main) 
	(letrec ((a (object))
			 (b #f))
		(set-property a 'g (object))
		(set! b (get-property a 'g))
		(foo a)
		(sink (get-property b 'f))))

(main)

; fig 3: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.359.8520&rep=rep1&type=pdf
; Highly Precise Taint Analysis for Android Application
; must for every run