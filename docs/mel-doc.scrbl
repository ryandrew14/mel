#lang scribble/manual

@; Document information
@title[#:tag "mel-doc" #:date "March 28, 2018"]{MEL}
@author["Da-Jin Chu and Ryan Drew"]
@section{Introduction}

Mel is a #lang for racket that provides basic tools to make music. The programmer can
define musical statements such as series of sounds and loops that are simultaneously played
upon running the Mel program. The programmer assigns pitches and rhythms to these statements
as well as the musical instrument with which to play the sounds.

@section{Program Structure}

Mel programs begin with a tempo statement, followed by a series of define or play statements.

@subsection{A Sample Program}

@codeblock{
 #lang mel

 (tempo 80)

 (define seqA '(1 2 3 4))
 (define seqB '(1 2))

 (define loop1
 (loop 4
       (player hihat seqB)))

 (play loop1)

 (play (loop 4 (player hihat seqA)))

 (play (loop 4 (reverb (player snare seqB))))

 (play (loop 4 (pitch Cmaj '(1 2 3 5) (player synth '(1 2 3 4)))))
}

@; Grammar
@subsection{Grammar}
@(racketgrammar*

  [program
   (tempo n) top-expr ...]

  [top-expr
   (define id expr)
   (play player-expr)]

  [expr
   N
   (list N ...)
   player-expr
   id]

  [player-expr
   (player instrument (list N ...))
   (pitch Key-name (list N ...) player-expr)
   (loop N player-expr)
   (reverb N player-expr)
   id]

  [instrument
   hihat
   bassdrum
   snare
   kick
   crash
   synth]

  )

@; Application
@defform[(p arg f1 ...)
         #:contracts([arg s-exp?][f1 procedure?])]{

 Applies the functions @racket[f1 ...] to @racket[arg] in left-to-right order.
 @(racketblock (p (a b (c d)) f1 f2)) is equivalent to
 @(racketblock ((compose f2 f1) a b (list c d)))
}

@; Lambda of a single s-Expression
@defform[(-> arg body)
         #:contracts([arg (or s-exp? id?)][body expression?])]{
 Takes a single shaped argument @racket[arg] (that is, an s-expression) and a @racket[body]
 expression and returns a procedure whose @racket[body] expression can use any identifiers
 bound in @racket[arg]. @racket[arg] may be an @racket[id] or it may be a shaped argument.
 If @racket[arg] is an @racket[id], @racket[->] will receive a list of the arguments it was
 called with (like @racket[(λ arg body ...)]). Should produce a list or an atom, and an atom
 is equivalent to a list of length 1. This is done because Pipelines functions 
 take their arguments and return their results as lists. 
}

@; How do functions work?
@section{Functions using Pipelines}

@subsection{Shaped Arguments}
While Racket functions take a series of arguments (conceptually similar to a list), Pipelines
functions take their arguments in arbitrary "shapes" (similar to S-expressions). This means
that if a value is passed to a function that expects an argument of a different shape,
the function will return an error.

@; How about calling Pipelines functions?
@subsection{Calling Pipelines Functions}
Pipelines functions can be applied in two ways. When inside a @racket[||] form, Pipelines functions
can be called using @racket[[p arg f1]] as documented above OR as Racket functions using
@racket[{f1 arg}]. When outside of a @racket[||] form, Pipelines functions can be called in either
way using normal parentheses.

@; Style and stuff
@section{A Note on Style}
We choose to write @racket[[p arg f1 ...]] with brackets rather than parentheses in order to
keep visual
consistency so that a pipeline of functions is always surrounded by square brackets and is easily
identifiable as such.