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
Mel programs run forever, no matter if there are play statements or not, and must be stopped
by the user.

@subsection{A Sample Program}

@codeblock{
 #lang mel

 (tempo 80)

 (define seqA '(1 2 3 4))
 (define seqB '(1 2))

 (define loop1
 (loop 4 (player hihat seqB)))

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
   (reverb player-expr)
   id]

  [instrument
   hihat
   bassdrum
   snare
   kick
   crash
   synth])
@section{Top-Level Expressions}

@defform[(tempo n)
         #:contracts([n nat?])]{

 Sets the tempo, in bpm, of the song to be played by the file.
 A tempo statement must be the first top-level form in a Mel file.
}

@defform[(define x body)
         #:contracts([x id?][body expr?])]{
 Works just like racket's @racket[(define x body)]. Can only define expressions available
 in Mel - player-exprs, natural numbers, lists of natural numbers, and ids.
}

@defform[(play p)
         #:contracts([p player-expr?])]{
 Plays the sound created by @racket[p].
}

@section{Player Expressions}

Mel comes with @racket[player-expression]s, expressions that all consist of either just the
player function or a series of effect functions wrapped around a player function.
@racket[Player-expression]s provide the mechanism for creating sounds in Mel.

@defform[(player i beats)
         #:contracts([i instrument?] [beats (list/c nat?)])]{
 The player function constructs a basic sound series, playing the given instrument sound
 on the beats specified by the second argument.
}

@subsection{Effect Functions}
Mel provides a few different functions to add audio effects to a player-expression. These
functions are player-expressions themselves and always take another player-expression
as their final argument.

@defform[(pitch Key-name pitches p)
         #:contracts([pitches (list/c nat?)] [p player-expr?])]{
 Takes the name of a key, list of pitches (which must be the same length as the list of beats
 in p), and a @racket[player-expression]. This function adds the given pitches to the corresponding
 beats of the given @racket[player-expression] p, where the numbers in the pitches list are the
 degrees of the desired notes in the given key.

 Does not pitch drum sounds.

 Key-name can be any basic major or minor key name, formatted as such: [Note-letter][key-type], where
 key-type is either maj or min. So, C major is Cmaj. All flat keys are represented by their
 corresponding sharp key. So, E-flat minor is D#min. There is a final key, called midi, which allows
 the user to input midi note numbers (e.g. 60 for middle C).
}

@defform[(loop n p)
         #:contracts([n nat?][p player-expr?])]{
 Takes a natural number meaning the amount of beats to loop and a @racket[player-expression]
 and loops that player-expression.
}

@defform[(reverb p)
         #:contracts([p player-expr?])]{
 Adds a reverb sound effect to a player-expression.                                        
}                                                         
