#lang scribble/manual

@title{Pixel Playground}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@(require (for-label racket/base racket/contract/base))

@(define (rtech . args)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

A 2-D multiplayer video game engine, client and server.

@section{Overview}

