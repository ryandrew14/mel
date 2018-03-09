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
; song-expr    = [e from e to e]
;              | [e at e]

(require (for-syntax syntax/parse)
         rsound)

(provide
 ; Override module begin
 (rename-out [mel-module-begin #%module-begin])

 ; Rsound sounds
 kick snare (rename-out
             [crash-cymbal crash]
             [o-hi-hat hihat]
             [clap-1 clap]) 
 
 ; Racket basics
 #%app #%datum #%top-interaction require)


(define-syntax mel-module-begin
  (syntax-parser
    [(_ ((~datum tempo) t)
        ((~datum sequence) x s-exp ...) ...
        ((~datum song) song-exp ...))
     #'(#%module-begin
        (tempo t)
        (sequence x s-exp ...) ...
        (song song-exp ...))]))

(define-syntax tempo
  (syntax-parser
    [(_ t:nat) #'t]))

(define-syntax sequence
  (syntax-parser
    [(_ x:id seq-exp ...)
     #:with val (assemble-sequence #'seq-exp)
     #'(define x (assemble val))]))

(define-for-syntax (assemble-sequence
  (
