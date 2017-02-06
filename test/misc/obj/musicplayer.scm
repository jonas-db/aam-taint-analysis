; Example of a music player (in android, java) as described in ACTEVE
; 
;	if (g==Stopped)
;		if (a==Play) g = Playing
;		else if (a==Skip) g = Skipping 
;		else skip
;	else
;		if (a==Stop) g = Stopped 
;		else skip
; g = global variable (current state), a = input variable (new state), skip = doing nothing
;
; Modelled using JavaScript: 
; window and document, which holds 3 buttons: start, stop, skip
; 3 events: start, stop, skip
; Event sequences all start with [window, document, ...] (denoted as *)
; because these has to be loaded first before any event can be executed
; e.g.[*, play, stop] will be generated (see logic)
; e.g. [*, stop, ...] will not be generated because in the beginning the global state is STOPPED

(define (print message) (display message) (newline))

; Start button
(define button-start (object))
(define-data-property button-start "_enabled" #t)
(define-accessor-property button-start "enabled"
  (lambda () (get-property button-start "_enabled"))
  (lambda (v) (let ((current (get-property button-start "_enabled"))) ; Conservative updating!
                (if (not (= v current)) (set-property button-start "_enabled" v)))))

; Skip button
(define button-skip (object))
(define-data-property button-skip "_enabled" #t)
(define-accessor-property button-skip "enabled"
  (lambda () (get-property button-skip "_enabled"))
  (lambda (v) (let ((current (get-property button-skip "_enabled"))) ; Conservative updating!
                (if (not (= v current)) (set-property button-skip "_enabled" v)))))

; Stop button, default disabled because we are in stopped mode
(define button-stop (object))
(define-data-property button-stop "_enabled" #f)
(define-accessor-property button-stop "enabled"
  (lambda () (get-property button-stop "_enabled"))
  (lambda (v) (let ((current (get-property button-stop "_enabled"))) ; Conservative updating!
                (if (not (= v current)) (set-property button-stop "_enabled" v)))))

; music player states
(define STATE_STOPPED 0)
(define STATE_PLAYING 1)
(define STATE_SKIPPING 2)

; The logic of the music player
; TODO: update GUI accordingly
(define music-player-logic
  (lambda (a)
    (let ((g (get-property music-player "_state")))
      (if (= g STATE_STOPPED)
          ; State == stopped
          (if (= a STATE_PLAYING)
          	(begin
              (set-property music-player "_state" a)
              (set-property button-start "enabled" #t)
              (set-property button-skip "enabled" #t)) ; a == playing
              ; a == skipping else skip
              ; skipping only changes the state, nothing is updated
            (if (= a STATE_SKIPPING) (set-property music-player "_state" a))) 
          ; State != stopped
          (if (= (set-property music-player "_state" a))))))) ; a == stopped else skip

; music player object holding the logic
(define music-player (object))
(define-data-property music-player "_state" STATE_STOPPED)
(define-accessor-property music-player "state"
  (lambda () (get-property music-player "_state"))
  music-player-logic)

; document
(define document (object))
(define-data-property document "_loaded" #f)
(define-accessor-property window "load"
  (lambda () (get-property window "_loaded"))
  (lambda (v) (begin
				; add dom elements
				(define-data-property document "button-stop" button-stop)
				(define-data-property document "button-start" button-start)
				(define-data-property document "button-skip" button-skip)

                (print "document loaded")))) ; User logic follows after this



; window
(define window (object))
(define-data-property window "document" document)
(define-data-property window "_loaded" #f)
(define-accessor-property window "load"
  (lambda () (get-property window "_loaded"))
  (lambda (v) (begin (print "window loaded")
                     ; set window loaded
                     (set-property window "_loaded" v)                     
                     ; Fire document load event
                     (set-property (get-property window "document") "load" v))))

; Fire the load event of the window
(set-property window "load" #t)