(letrec ((id (lambda (i) i))) 
	(id #f) 
	(letrec ((x (id #t))) 
	(if x 'ok 'fail)))