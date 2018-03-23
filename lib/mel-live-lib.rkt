#lang racket

(require rsound)

(module+ test
         (require rackunit))

(provide
  update-tempo
  play-song
  (rename-out 
    [s-bassdrum bassdrum]))

;; Data Definitions

; A Song is a [Listof Loop]
; A Loop is a [N -> [Listof Sound]]
; A Sound is one of:
;  - hihat
;  - kick
;  - snare
;  - bassdrum
;  - crash

; INTEPRETATION
; A Song represents the overarching piece of "music"
; A Loop represents a single track of the song. It specifies some sounds for each beat of the song.
; A Sound is the atomic sound data.


(define tempo 60)

(define stream (make-pstream))
(define beat-length (* (/ (default-sample-rate) tempo) 60))

; N -> (void)
; Update the tempo and beat length
; EFFECT sets the tempo and beat length.
(define (update-tempo t)
  (set! tempo t)
  (set! beat-length (* (/ (default-sample-rate) current-tempo) 60)))

; Sound N -> (void)
; Queues a sound onto the stream to play at a certain beat number.
(define (play-at-beat sound beat-num)
  (begin
    (pstream-queue stream sound (round (* beat-length beat-num)))
    (void)))

; N -> N
; Calculate the beat that the specified frame belongs to.
(module+ test
         (check-equal? (frame->beat 100)
                       (floor (/ 100 beat-length))))
(define (frame->beat frame)
  (floor (/ frame beat-length)))

; N -> N
; Calculate the first frame of the specified beat.
(module+ test
         (check-equal? (beat->frame 2)
                       (round (* 2 beat-length))))
(define (beat->frame beat)
  (round (* beat beat-length)))

; -> N
; Get the current beat number.
(define (current-beat)
  (frame->beat (pstream-current-frame stream)))

; -> N
; Get the frame number of the next down beat.
(define (next-beat-frame)
  (beat->frame (add1 (current-beat))))

(define s-bassdrum (rs-scale .5 bassdrum))

; QUEUE-SIZE defines how many beats to queue at once.
(define QUEUE-SIZE 3)

; QUEUE-AHEAD defines how far in advance to queue sounds, in beats.
(define QUEUE-AHEAD 3)

; Song -> (void)
; play-song runs indefinitely, playing the specified song.
; EFFECT plays sounds FOREVER
(define (play-song song)
    (for ([i QUEUE-SIZE])
        (play-at-beat ts (+ QUEUE-AHEAD i (current-beat))))
    (sleep (/ (- (beat->frame (+ 3 (current-beat))) (pstream-current-frame stream)) (default-sample-rate)))
    (play-song song))

