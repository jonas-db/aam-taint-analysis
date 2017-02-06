; abstract counting objectstore : true
; define data property extends the store with the next object
; the var a points to two different addresses in the object store
;(define a (object))
;(define-data-property a "data" #t)
;(set! a (object))
;(define-data-property a "id" 1)
;(get-property a "data")

(define a (object))
(define-data-property a "same" (taint #t))
(define-data-property a "same" "ok")

(sink (get-property a "same")) ; f: string/#t, t:string