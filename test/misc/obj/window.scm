; Example of a form, where the send button is enabled after both both inputs has been set or "validated"
; Because the user can first fill in the age and then the name, or the other way around
; we have to "fire" an event to tell the button that it might have to enable itself,
; send can be seen as the "unload" event, i.e. no more events possible after that event/denotes end of a sequence
; in the end, we have 2 different event sequences (combination of name/age filling) that can enable the send  button
; [input name]
; [input age]
; [button SEND]
; Exists of 4 objects: window, document, button and person (which represents the form with fields name age)
; seq of length 4:
; Possible seqs: [window name age send], [window age name send]
; Impossible: [window name send ..] [name send ..] [send ..] (send is not enabled at those times)
;             [ name window ...] (window not loaded yet etc)
; possible augmentation: fields can be reset e.g. [window age reset_age name age send]
;
; OR: each object/event has a property "enabled" depending on program logic they get enabled or not
; e.g. when the window object is enabled, it enables all inputs fields ("dom objects")
; traverse through the program, building a "dependency" graph which indicates which event can enable an event
; based on the graph, calculate the sequences..

(define (print message) (display message) (newline))

(define button (object))
(define-data-property button "_enabled" #f)
(define-data-property button "_counter" 0)

(define (check-button v) 
  (begin (print "check button")
         (let ((counter (get-property button "counter")))
           (if (eq? v #t)
               (set-property button "counter" (+ counter 1))    ; input was filled in
               (set-property button "counter" (- counter 1)))))) ; (UNUSED ATM) input was deleted (i.e. blank)			

(define-accessor-property button "enabled"
  (lambda () (get-property button "_enabled"))
  check-button)

(define-accessor-property button "counter"
  (lambda () (get-property button "_counter"))
  (lambda (v) (if (= v 2)
                  (begin (set-property button "_enabled" #t) (print "button enabled"))
                  (begin (print "not yet enabled..")(set-property button "_counter" v)))))

(define person (object))
(define-data-property person "_name" #f)
(define-data-property person "_age" #f)

; represents the name input field
(define-accessor-property person "name"
  (lambda () (get-property person "_name"))
  (lambda (v) (begin
  				; TODO: check if window is loaded?
                (set-property person "_name" v)
                (print "setting name")
                (set-property button "enabled" #t) )))

; represents the age input field
(define-accessor-property person "age"
  (lambda () (get-property person "_age"))
  (lambda (v) (begin
  				; TODO: check if window is loaded?
                (set-property person "_age" v)
                (print "setting age")
                (set-property button "enabled" #t) )))	

(define window (object))
(define-data-property window "_loaded" #f)

(define-accessor-property window "load"
  (lambda () (get-property window "_loaded"))
  (lambda (v) (begin (print "window loaded") 
                     (set-property window "_loaded" v)
                     
                     ; SEND EVENTS
                     (set-property person "name" (taint))
                     (set-property person "age" 24))))

(define document (object))
(define-data-property window "document" document)
(define-data-property document "button" button)

; The load event fires at the end of the document loading process
; any event before this event is NOT possible
(set-property window "load" #t)

(get-property person "name")
;-f test/obj/window.scm -m AAM -l Test -d fact.dot
;-f test/obj/window.scm -m AAM -l Concrete -c -d fact.dot

 ; we can simply hardcode all properties in a "handle" function with a switch case 
 ; and manually return the specific element for a given property 
 ; e.g. handle(proper) switch(prop) case "button": return (get-property document "button")
 ; in this way we don't need any dynamic properties
;(define-data-property document "getElementById" get-element-by-id)

; NOT NEEDED, look above ^
; TODO: ask "propertylattice" that only takes strings as concrete and other types as in type lattice
; such that i can also use dynamic property keywords
; such that (get-property button (get-property obj "name")) would work, where (get-property obj "name") returns a string
; such that i simply can evaluate it with an extra (frame)step and then do a "hack" as in Abs.toString() which returns the concrete string