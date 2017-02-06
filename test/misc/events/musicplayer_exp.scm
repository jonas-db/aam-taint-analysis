; Example of a music player (in android, java) as described in ACTEVE
; 
; g = global variable (current state), a = input variable (new state), skip = doing nothing
;
; Modelled using JavaScript: 
; window and document, which holds 3 buttons: start, stop, skip [play >] [stop] [ skip >> ]
; 3 events: start, stop, skip
; Event sequences all start with [window, document, ...] (denoted as *)
; because these has to be loaded first before any event can be executed
; e.g.[*, play, stop] will be generated (see logic)
; e.g. [*, stop, ...] will not be generated because in the beginning the global state is STOPPED

(define (print message) (display message) (newline))

; Start button
(define button-start (object))

; Skip button
(define button-skip (object))

; Stop button
(define button-stop (object))

; music player states
(define STATE_STOPPED 0)
(define STATE_PLAYING 1)
(define STATE_SKIPPED 2)

(define song 0)

; The logic of the music player
(define music-player-logic
  (lambda (a)
    (let ((g (get-property music-player "state")))
      (if (= g STATE_STOPPED)
          (if (= a STATE_PLAYING) 
	          (begin
	          	(set-property music-player "state" a))
	          	; EXPLICITLY remove the event listener of stop button, i.e. disable the start button
	          	(remove-event-listener button-start "click" #f) 
	          	; EXPLICITLY add the event listener of stop button, i.e. enable the stop button again
	          	(add-event-listener button-stop "click" 
	          		(lambda (event) (set-property music-player "_state" STATE_STOPPED))))
	       ; else a == pauzing/stopped -> do nothing
      )
      (if (= g STATE_PLAYING)
          (if (!= a STATE_PLAYING)
	          (begin
	          	(set-property music-player "state" a))
	          	; EXPLICITLY remove the event listener of stop button, i.e. disable the stop button
	          	(remove-event-listener button-stop "click" #f) 
	          	; EXPLICITLY add the event listener of start button, i.e. enable the start button again
	          	(add-event-listener button-start "click" 
	          		(lambda (event) (set-property music-player "_state" STATE_PLAYING))))    
          	; else a == started -> do nothing
      )
      (if (= g STATE_SKIPPED) (set! song (+ song 1))) ; next song, can happen while playing/stopped     
      )))

; music player object holding the logic
(define music-player (object))
(define-data-property music-player "_state" STATE_STOPPED)
(define-accessor-property music-player "state"
  (lambda () (get-property music-player "_state"))
  music-player-logic)

; document
(define document (object))
(define-data-property document "_loaded" #f)
(add-event-listener document "load" (lambda (event)
	(begin
		; add dom elements
		(define-data-property document "button-stop" button-stop)
		
		(define-data-property document "button-start" button-start)
		(add-event-listener button-start "click" (lambda (event) (set-property music-player "_state" STATE_PLAYING) ))
		
		(define-data-property document "button-skip" button-skip)
		(add-event-listener button-skip "click" (lambda (event) (set-property music-player "_state" STATE_SKIPPED) ))

        ; remove listener
        (remove-event-listener document "load" #f)

         ; set document loaded
        (set-property document "_loaded" #t)

        (print "document loaded"))))

; window
(define window (object))
(define-data-property window "document" document)
(define-data-property window "_loaded" #f)
(add-event-listener window "load" (lambda (event)
	(begin 
		(print "window loaded")
         ; set window loaded
        (set-property window "_loaded" #t)
        ; remove listener
        (remove-event-listener window "load" #f)                     
         ; Fire document load event
        (dispatch-event document "load" #f))))

; final event loop
(event-loop)