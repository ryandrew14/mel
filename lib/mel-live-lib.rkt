#lang racket

(require rsound)

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

; Sound N -> (void)
; Queues a sound onto the stream to play at a certain beat number.
(define (play-at-beat sound beat-num)
  (begin
    (pstream-queue stream sound (round (* beat-length beat-num)))
    (void)))

; N -> N
; Calculate the beat that the specified frame belongs to.
(define (frame->beat frame)
  (floor (/ frame beat-length)))

; N -> N
; Calculate the first frame of the specified beat.
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


(define ts (rs-scale .5 bassdrum))

; Song -> (void)
; play-song runs indefinitely, playing the specified song.
(define (play-song song)
    (play-at-beat ts (+ 3 (current-beat)))
    (play-at-beat ts (+ 4 (current-beat)))
    (play-at-beat ts (+ 5 (current-beat)))
    (sleep (/ (- (beat->frame (+ 3 (current-beat))) (pstream-current-frame stream)) (default-sample-rate)))
    (play-song song))
(play-song 's)
