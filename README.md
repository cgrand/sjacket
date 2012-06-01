# Sjacket

Sjacket stated goal is to become a common backend for Clojure editors.

It provides:
* cheap, always up-to-date, parse trees and views
* structural transformations by simple sexpr transformations (you don't have to
  care about the source layout and comments, sjacket does its best to preserve
  them) 

Sjacket is still in its infancy: despite having thought about it for a while
https://twitter.com/cgrand/status/165471721531506689, I only wrote some code
in a TDD (Talk Driven Development) fashion for EuroClojure 2012 
(see Not So Homiconic.pdf for the slides).

The short term goal is to provide a demo ui which will serve as an example for
integrators. Once the demo ui is done, work on the transformations will resume.

