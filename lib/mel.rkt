#lang racket

; Program
;      p       = (tempo x) (sequence x [seq-expr] ...) ... s

; Number
;      n       = Number
     
; Expressions
;      e       = x
;              | kick
;              | snare
;              | crash
;              | hihat
;              | clap

; Song
;      s       = (song song-expr ...)

; Sequence-Expression
; seq-expr     = (play e play-option ...)
;              | (rest n)

; Play-option
; play-option  = #:repeat n
;              | #:stretch n
;              | #:rate n
;              | #:amp n
;              | #:attack n
;              | #:release n
;              | #:sustain n
;              | #:pan n

; Song-Expression
; song-expr    = [x from n to n]
;              | [x at n]

(require (for-syntax syntax/parse)
         rsound)

(provide
 ; Override module begin
 (rename-out [mel-module-begin #%module-begin])
 
 tempo
 sequence
 song
 ; Rsound sounds
 (rename-out
  [s-kick kick]
  [s-snare snare]
  [s-bassdrum bassdrum]
  [s-crash crash]
  [s-hihat hihat]
  [clap-1 clap]) 
 
 ; Racket basics
 #%datum #%top-interaction require)

;; Scaled Sounds
(define s-kick (rs-scale .5 kick))
(define s-snare (rs-scale .5 snare))
(define s-bassdrum (rs-scale .7 bassdrum))
(define s-crash (rs-scale .5 crash-cymbal))
(define s-hihat (rs-scale .5 o-hi-hat))

;; Tempo declaration (will be set! by tempo statement)
(define current-tempo 0)

;; Value of 1 beat
(define beat-length 0)

;; Song declaration (will be added to by song statement)
(define final-song (make-pstream))

(define-syntax mel-module-begin
  (syntax-parser
    [(_ ((~datum tempo) t)
        ((~datum sequence) x s-exp ...) ...
        ((~datum song) song-exp ...))
     #'(#%module-begin
        (tempo t)
        (sequence x s-exp ...) ...
        (song song-exp ...))]
    [(_) (error "Program does not follow grammar: (tempo x) (sequence x [seq-expr] ...) ... s")]))

;; EFFECT sets current-tempo to t
(define-syntax tempo
  (syntax-parser
    [(_ t:nat) #'(update-tempo t)]))

(define-syntax sequence
  (syntax-parser
    [(_ x:id seq-exp ...)
     #:with val (assemble-sequence (syntax->list #'(seq-exp ...)))
     #'(define x (assemble val))]))

;; [Listof seq-expr] -> [Listof [Pair rsound N]]
(define-for-syntax (assemble-sequence exp)
  (foldl get-seq-info #''() exp))

;; seq-expr [Listof [Pair rsound N]] -> [Listof [Pair rsound N]]
(define-for-syntax (get-seq-info e lop)
  (syntax-parse e
    [((~datum rest))
     #`(cons (list (silence beat-length) (beat->frame (length #,lop))) #,lop)]
    [((~datum play) rs)
     #`(cons (list rs (beat->frame (length #,lop))) #,lop)]))

(define-syntax song
  (syntax-parser
    [(_ song-expr ...)
     #'(begin (add-to-queue song-expr) ...)]))

(define-syntax add-to-queue
  (syntax-parser
    [(_ [rs:id (~datum at) n:nat])
     #'(pstream-queue final-song rs (beat->frame n))]))

;; Runtime tempo helpers:

;; Converts beat number to frame value
(define (beat->frame bn)
  (round (* bn beat-length)))

;; Update the tempo and beat length
(define (update-tempo t)
  (set! current-tempo t)
  (set! beat-length (* (/ (default-sample-rate) current-tempo) 60)))
