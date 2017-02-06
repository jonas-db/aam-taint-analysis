(let* ((g (lambda (y) (car y)))
       (f (lambda (x) (g x))))
  (f (cons 'foo "bar"))
  (f (cons 2 #t)))

