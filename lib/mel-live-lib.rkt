#lang racket

(require rsound)
(require rsound/piano-tones)

(module+ test
  (require rackunit))

(provide
  (contract-out
    ; Update the tempo and beat length
    ; EFFECT sets the tempo and beat length.
    [update-tempo (-> integer? void?)]

    ; play-song runs indefinitely, playing the specified song.
    ; EFFECT plays notes FOREVER
    [play-song (-> song? void?)]

    ; N Player -> Player
    ; Loops a player using the specified loop length.
    [loop (-> integer? player? player?)]

    ; Instrument [Listof N] -> Player
    ; Creates a player object given a note when to play the note.
    [make-player (-> instrument? (listof integer?) player?)])

  (rename-out
    [s-kick kick]
    [s-snare snare]
    [s-bassdrum bassdrum]
    [s-crash crash]
    [s-hihat hihat]))

; Data Definitions

(define-struct note [instrument pitch effects] #:transparent)
(define-struct player [func beats] #:transparent)

; A Beat is a positive integer
; A Pitch is a positive integer
; An Rsound is from the rnote library

; A Song is a [Listof Player]
; A Player is a (player [Beat -> [Maybe Note]] [Listof Beat])
; A Note is a (note Instrument Pitch [Listof SoundEffect])
; An Instrument is a [Pitch -> Rsound]
; A SoundEffect is a [Rsound -> Rsound]

; INTEPRETATION
; Beat represents the moments of time in the song.
; Pitch represents the frequency of a note.

; Song represents the overarching piece of "music."
; Player represents a single track of the song. It specifies some notes for each beat of the song.
; Note represents the individual notes that make up the song.
; Instrument represents an musical instrument or synthesizer that can be played at different pitches.
;    Some instruments have no effect when their pitch is changed.
; SoundEffect represents a modification to an Rsound.

;; Constants

; Define instruments

; Rsound -> Instrument
; Create a instrument that always return the rnote, scaled.
(define (rs->instrument rs)
  (define s-rs (rs-scale .3 rs))
  (lambda (p)
    s-rs))

(define s-bassdrum (rs->instrument bassdrum))
(define s-kick (rs->instrument kick))
(define s-snare (rs->instrument snare))
(define s-crash (rs->instrument crash-cymbal))
(define s-hihat (rs->instrument o-hi-hat))

(define synth 
  (lambda (p)
    (synth-note "main" 1 (+ p 59)  beat-length)))

(define tempo 100)

(define stream #f)
(define beat-length (* (/ (default-sample-rate) tempo) 60))

;; Predicates

; Any -> Boolean?
; Is this a song?
(define (song? s)
  (andmap player? s))

; Any -> Boolean?
; Is this an instrument?
(define (instrument? i)
  (and (procedure? i) (= 1 (procedure-arity i))))

; N -> (void)
; Update the tempo and beat length
; EFFECT sets the tempo and beat length.
(define (update-tempo t)
  (set! tempo t)
  (set! beat-length (* (/ (default-sample-rate) tempo) 60)))

; Note -> Rsound
; Turns the note struct into an rsound
(module+ test
  (check-equal? (note->rs (note s-bassdrum 1 (list identity)))
                (s-bassdrum 1)))
(define (note->rs s)
  ((apply compose (note-effects s))
   ((note-instrument s) (note-pitch s))))

; Note N -> (void)
; Queues a note onto the stream to play at a certain beat number.
; Beat-num should NOT be 0! 
; The pstream plays upon creation, so it is impossible to hear notes at beat 0.
(define (play-at-beat snd beat-num)
  (if (positive? beat-num)
    (begin
      (pstream-queue stream (note->rs snd) (beat->frame beat-num))
      (void))
    (error "Cannot play notes at beat-num 0"))) 

; Player N -> (void)
; Queues the notes of a loop at a specified beat number onto the stream.
(define (play-player-at-beat p beat-num)
  (if ((player-func p) beat-num)
    (play-at-beat ((player-func p) beat-num) beat-num)
    (void)))

; N -> N
; Calculate the beat that the specified frame belongs to.
; Frame 0 corresponds to beat 0. Frame <beat-length> is beat 1.
; There should NOT be anything playing at beat 0!
(module+ test
  (check-equal? (frame->beat 100)
                (floor (/ 100 beat-length))))
(define (frame->beat frame)
  (floor (/ frame beat-length)))

; N -> N
; Calculate the first frame of the specified beat.
(module+ test
  (check-equal? (beat->frame 1)
                (round beat-length))
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

; QUEUE-SIZE defines how many beats to queue at once.
(define QUEUE-SIZE 4)

; QUEUE-AHEAD defines how far in advance to queue notes, in beats.
(define QUEUE-AHEAD 5)

; Song N -> (void)
; Queues all notes in a song for a given beat.
(define (play-song-at-beat song beat)
  (for ([player song])
    (play-player-at-beat player beat)))

; Song -> (void)
; play-song runs indefinitely, playing the specified song.
; EFFECT plays notes FOREVER
(define (play-song song)
  (set! stream (make-pstream))
  (for ([i (in-range 1 (add1 QUEUE-AHEAD))])
    (play-song-at-beat song i))
  (let loop ([last-queued QUEUE-AHEAD])
    (define queue-to (+ (current-beat) QUEUE-SIZE QUEUE-AHEAD))
    (for ([i (in-range (+ 1 last-queued) (+ 1 queue-to))])
      (play-song-at-beat song i))
    (sleep (/ (- (beat->frame (+ QUEUE-SIZE (current-beat))) (pstream-current-frame stream)) (default-sample-rate)))
    (loop queue-to)))


; Instrument [Listof N] -> Player
; Creates a player object given a note when to play the note.
(module+ test
  (define p (player-from-instrument s-bassdrum '(1 3)))
  (check-equal? (note->rs ((player-func p) 1)) (s-bassdrum 0))
  (check-equal? ((player-func p) 2) #f)
  (check-equal? (note->rs ((player-func p) 3)) (s-bassdrum 0))
  (check-equal? ((player-func p) 8) #f))
(define (player-from-instrument instrument play-times)
  (player
    (lambda (beat)
      (define should-play (member beat play-times))
      (if should-play
        (note instrument 0 '())
        #f))
    play-times))

; [Listof X] [Listof Beat] Beat -> [Maybe X]
; Returns the X in the list of X corresponding to the given Beat in the list of Beats.
(module+ test
  (check-equal? (beat-lookup '(5 6 7) '(1 3 5) 1) 5)
  (check-equal? (beat-lookup '(5 6 7) '(1 3 5) 3) 6)
  (check-equal? (beat-lookup '(5 6 7) '(1 3 5) 5) 7)
  (check-equal? (beat-lookup '(5 6 7) '(1 3 5) 6) #f))
(define (beat-lookup lox lob b)
  (cond
    [(or (empty? lox)(empty? lob)) #f]
    [(= (first lob) b) (first lox)]
    [else (beat-lookup (rest lox) (rest lob) b)]))

; [Listof Pitch] Player -> Player
; Sets the pitches of the player
(define (set-pitch pitches p)
  (player
    (lambda (b)
      (define l-note ((player-func p) b))
      (if l-note
        (note (note-instrument l-note)
              (beat-lookup pitches (player-beats p) b)
              (note-effects l-note))
        #f))
    (player-beats p)))

; N Player -> Player
; Loops a player using the specified loop length.
(module+ test
  (define simple (loop 4 p))
  (check-equal? (note->rs ((player-func simple) 1)) (s-bassdrum 0))
  (check-equal? ((player-func simple) 2) #f)
  (check-equal? (note->rs ((player-func simple) 53)) (s-bassdrum 0))
  (check-equal? ((player-func simple) 24) #f))
(define (loop loop-len p)
  (player
    (lambda (n)
      (define mod-beat (add1 (modulo (sub1 n) loop-len)))
      ((player-func p) mod-beat))
    (player-beats p)))


#;
(play-song (list (loop 4 (player-from-instrument s-hihat '(1 2 3 4)))
                 (loop 4 (player-from-instrument s-snare '(1 2)))
                 (loop 4 (player-from-instrument s-bassdrum '(1 2)))))

(play-song (list (set-pitch '(1 3 4 12 6 7 8 9) (player-from-instrument synth '(1 2 3 4 5 6 7 8)))))
