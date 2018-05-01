# reflex-dom-inbits - A beginner friendly step by step tutorial for reflex-dom

[![License BSD3][badge-license]][license]

[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/hansroland/reflex-dom-inbits/blob/master/LICENSE


This is a beginner-friendly tutorial for *reflex-dom*. It shows how to write Haskell programs  with a graphical user interface.
It contains an introductionary text and examples in short bits.

Reflex-Dom is a library to create Haskell applications with a Graphical User Interface (GUI).
The GUI is based on the popular Document Object Model (DOM) that is used in the Internet Browsers.
Therefore Reflex-Dom programs can be run in the Web Browser or as a Webkit-Gtk application. 

*Reflex-dom* is based on *Reflex*, a Haskell implementation of Functional Reactive Programming.

It's not necessary to be a Haskell guru to follow this tutorial. 
A basic understanding of Haskell and the concepts of *Functor*, *Applicative* and *Monad* is enough.
With Reflex-Dom you can write GUI applications in Haskell without understanding the concepts of
*State Monad* or *Monad Transformers*. You need also a basic background on *HTML* and 
on *Cascaded Style Sheets* (*CSS*). Of course, the more experience you have, the easier it is.

Start by cloning this repository with ``` git clone https://github.com/hansroland/reflex-dom-inbits ```.
Continue by installing Reflex.Dom. The preferred installation method is to use
the reflex-platform from [https://github.com/reflex-frp/reflex-platform](https://github.com/reflex-frp/reflex-platform).
Alternatively you can use stack, however, this will take a long time. If you use stack, I recommend
to use version 1.24.0.2 of cabal. I was unable to run _stack setup_ with cabal 2.0.0.0.

Then read the file [tutorial.md](tutorial.md).