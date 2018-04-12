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

  ; Instrument [Listof N] -> Player
  ; Creates a player object given an instrument and when to play the instrument.
  [player-from-instrument (-> instrument? (listof integer?) player?)]

  ; N Player -> Player
  ; Loops a player using the specified loop length.
  [set-loop (-> integer? player? player?)]

  ; Key [Listof Pitch] Player -> Player
  ; Sets the pitches of the player.
  [set-pitches (-> (listof integer?) (listof integer?) player? player?)]

  ; Player -> Player
  ; Sets reverb to the player.
  [set-reverb (-> player? player?)]

  ; Number Player -> Player
  ; Sets the amplitude of the player.
  [set-amp (-> number? player? player?)]

  ; [Listof Number] Player -> Player
  ; Sets the amplitude of each note of the player 
  [set-amps (-> (listof number?) player? player?)])

 ; Keys
 Cmaj C#maj Dmaj D#maj Emaj Fmaj F#maj Gmaj G#maj Amaj A#maj Bmaj 
 Cmin C#min Dmin D#min Emin Fmin F#min Gmin G#min Amin A#min Bmin 
 midi

 (rename-out
  [s-kick kick]
  [s-snare snare]
  [s-bassdrum bassdrum]
  [s-crash crash]
  [s-hihat hihat])
 synth2
 synth

 tempo)

; Data Definitions

(define-struct note [instrument pitch effects] #:transparent)
(define-struct player [func beats] #:transparent)

; A Beat is a positive integer
; Interpretation: represents a beat number
; A Duration is a positive integer
; Interpretation: represents the duration a pitch will be played for
; A Pitch is a positive integer
; Interpretation: represents a pitch (either by degree or by midi note depending on context)
; An Rsound is from the rnote library

; A Song is a [Listof Player]
; A Player is a: (player [Beat -> [Maybe Note]] [Listof Beat])
; A Note is a (note Instrument Pitch [Listof SoundEffect])
; An Instrument is a [Pitch -> Rsound]
; A SoundEffect is a [Rsound -> Rsound]
; A Key is one of:
; - [Listof Pitch] with length 7
; - '()
; A Key-Type is one of:
; - 'major
; - 'minor

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
    (synth-note "main" 22 p (round beat-length))))

(define synth2
  (lambda (p)
    (synth-note "main" 5 p (round beat-length))))

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
  (define a-bassdrum (note s-bassdrum 1 (list identity)))
  (define b-bassdrum (note s-bassdrum 1 (list rs-reverb)))
  (check-equal? (note->rs a-bassdrum)
                (s-bassdrum 1))
  (check-equal? (note->rs b-bassdrum)
                (rs-reverb (s-bassdrum 1))))
(define (note->rs s)
  ((apply compose (note-effects s))
   ((note-instrument s) (note-pitch s))))

; [Beat -> [Maybe Note]] [Listof Duration] -> Player
; Creates a player using a list of durations instead of a list of beats.
#;
(define (player-dur func lod)
  (player func (beats-from-durations lod)))

; [Listof Duration] -> [Listof Beat]
; Creates a list of beats corresponding to the

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
    (sleep (/ (- (beat->frame (+ QUEUE-SIZE (current-beat))) (pstream-current-frame stream))
              (default-sample-rate)))
    (loop queue-to)))

;; Define provided player constructor and modifiers

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
         (note instrument 60 '())
         #f))
   play-times))

; N Player -> Player
; Loops a player using the specified loop length.
(module+ test
  (define simple (set-loop 4 p))
  (check-equal? (note->rs ((player-func simple) 1)) (s-bassdrum 0))
  (check-equal? ((player-func simple) 2) #f)
  (check-equal? (note->rs ((player-func simple) 53)) (s-bassdrum 0))
  (check-equal? ((player-func simple) 24) #f))
(define (set-loop loop-len p)
  (struct-copy 
   player 
   p
   [func (lambda (n)
           (define mod-beat (add1 (modulo (sub1 n) loop-len)))
           ((player-func p) mod-beat))]))

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

; Key [Listof Pitch] Player -> Player
; Sets the pitches of the player
(define (set-pitches key pitches p)
  (if (= (length pitches) (length (player-beats p)))
      (let ([keyed-pitches (map (λ (x) (pitch-in-key key x)) pitches)])
        (struct-copy 
         player 
         p
         [func (lambda (b)
                 (define l-note ((player-func p) b))
                 (define l-pitch (beat-lookup keyed-pitches (player-beats p) b))
                 (if (and l-note l-pitch)
                     (struct-copy 
                      note 
                      l-note
                      [pitch l-pitch])
                     l-note))]))
      (raise-arguments-error 
       'set-pitches 
       "List of pitches must be same length as player's beats" 
       "pitches" pitches
       "p" p)))

; Key Pitch -> Pitch
; given a relative pitch to a key, returns the proper midi note
(module+ test
  (check-equal? (pitch-in-key (make-key 'major 60) 1) 60)
  (check-equal? (pitch-in-key (make-key 'major 60) 3) 64)
  (check-equal? (pitch-in-key (make-key 'minor 60) 10) 75)
  (check-equal? (pitch-in-key '() 60) 60))
(define (pitch-in-key key p)
  (define p-adjusted (- p 1))
  (cond
    [(empty? key) p]
    [else
     (cond
       [(< p-adjusted 7) (list-ref key p-adjusted)]
       [(>= p-adjusted 7) (+ 12 (pitch-in-key key (- p 7)))])]))

; Symbol Pitch -> Key
; Makes a key, given the midi note to start on
(module+ test 
  (check-equal? (make-key 'major 60) '(60 62 64 65 67 69 71))
  (check-equal? (make-key 'major 62) '(62 64 66 67 69 71 73))
  (check-equal? (make-key 'minor 60) '(60 62 63 65 67 68 70)))
(define (make-key key-type n)
  (define add-n (lambda (x) (+ n x)))
  (cond
    [(symbol=? key-type 'major)
     (map add-n '(0 2 4 5 7 9 11))]
    [(symbol=? key-type 'minor)
     (map add-n '(0 2 3 5 7 8 10))]))

; Predefined Keys
(define midi '())
(define Cmaj (make-key 'major 60))
(define C#maj (make-key 'major 61))
(define Dmaj (make-key 'major 62))
(define D#maj (make-key 'major 63))
(define Emaj (make-key 'major 64))
(define Fmaj (make-key 'major 65))
(define F#maj (make-key 'major 66))
(define Gmaj (make-key 'major 67))
(define G#maj (make-key 'major 68))
(define Amaj (make-key 'major 69))
(define A#maj (make-key 'major 70))
(define Bmaj (make-key 'major 71))
(define Cmin (make-key 'minor 60))
(define C#min (make-key 'minor 61))
(define Dmin (make-key 'minor 62))
(define D#min (make-key 'minor 63))
(define Emin (make-key 'minor 64))
(define Fmin (make-key 'minor 65))
(define F#min (make-key 'minor 66))
(define Gmin (make-key 'minor 67))
(define G#min (make-key 'minor 68))
(define Amin (make-key 'minor 69))
(define A#min (make-key 'minor 70))
(define Bmin (make-key 'minor 71))

; Player -> Player
; Adds reverb to the player.
(define (set-reverb p)
  (struct-copy 
   player 
   p
   [func (lambda (b)
           (define l-note ((player-func p) b))
           (if l-note
               (add-effect rs-reverb l-note)
               l-note))]))

; Number Player -> Player
; Sets the amplitude of the player 
; Vol should be between 0 and 1.
(define (set-amp vol p)
  (struct-copy
   player
   p
   [func (lambda (b)
           (define l-note ((player-func p) b))
           (if l-note
               (add-effect (mk-amp-effect vol) l-note)
               l-note))]))

; [Listof Number] Player -> Player
; Sets the amplitude of each note of the player 
; Vol should be between 0 and 1.
(define (set-amps vols p)
  (struct-copy
   player
   p
   [func (lambda (b)
           (define l-note ((player-func p) b))
           (define vol (beat-lookup vols (player-beats p) b))
           (if (and l-note vol)
               (add-effect (mk-amp-effect vol) l-note)
               l-note))]))

; SoundEffect Note -> Note
; Adds the soundeffect to the note
(module+ test
  (check-equal? (note->rs (add-effect rs-reverb a-bassdrum))
                (note->rs b-bassdrum)))
(define (add-effect se a-note)
  (struct-copy note a-note
               [effects (cons se (note-effects a-note))]))

;; Defining Sound Effects

; Number -> SoundEffect
; Creates a SoundEffect that sets amplitude
; a should be between 0 and 1
(define (mk-amp-effect a)
  (lambda (rs)
    (rs-scale a rs)))

; Rsound -> Rsound
; Adds reverb to the actual rsound.
(define (rs-reverb rs)
  (signal->rsound
   (round beat-length)
   (network ()
            [a <= (rsound->signal/left rs)]
            [out <= reverb a])))

#;
(play-song (list (set-loop 4 (set-amps '(1 1 1 .3) (player-from-instrument s-hihat '(1 2 3 4))))
                 (set-loop 4 (player-from-instrument s-snare '(1 2)))
                 (set-loop 4 (player-from-instrument s-bassdrum '(1 2)))))

#;
(play-song (list (set-pitches (make-key 'major 60) '(1 2 3 4 5 6 7 8)
                              (player-from-instrument synth '(1 2 3 4 5 6 7 8)))
                 (set-pitches (make-key 'major 60) '(8 7 6 5 4 3 2 1)
                              (player-from-instrument synth '(1 2 3 4 5 6 7 8)))))


