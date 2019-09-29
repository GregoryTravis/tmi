TMI
====

## Overview

TMI is an experimental language that contains the following buzzwords:

1. functional
1. pure
1. relational
1. bidirectional
1. lens
1. FRP
1. concurrent
1. distributed
1. spreadsheet

In TMI, the entire lifetime of a software system is treated a single
time-series database.  Proceeding through the sequence of database states is
defined as the execution of the program.  That means that the development
process can be considered to happen during the execution of the program.

The database consists of all of the data in the program.  All individual data
elements are views of this database, and some of them are writable -- thus, it
is pervasively lens-y.

Below are some of the goals of the language.  Some of these are very clear in
my mind, and some of them are just hazy dreams.  I mark each one with how
likely I think it is that I'm on to something useful.

*(100%)* **Remove [accidental
complexity](https://en.wikipedia.org/wiki/No_Silver_Bullet):** Everyone already
knows this is important, almost by definition.

*(75%)* **Deincrementalization:** Yes, premature optimization is the root of
all evil, but more specifically I believe incrementalization is a particularly
significant source.  Most imperative code updates state step-by-step, instead
of re-calculating state from ground truth.  In almost any imperative program,
the various pieces of code that change any given data structure can be thought
of as multiple, redundant, and possibly inconsistent specifications of the
semantics of that data.  The problem got much worse when concurrency became
mainstream.  Thus, FRP.  Incrementalization isn't just for speed, it also has
tremendous power to simplify code.

*(80%)* **Pessimization:** This is the act of going to extremes to simplify
code and making its meaning obvious.  Again, premature optimization etc etc.
We all say we are going to write the simplest code, and then we don't.  I think
in order to really pull this off, you have to get to the point that you almost
feel like you're trying to make it slower.  Then optimize it not by rewriting
the code, but by adding optimizations that are guaranteed not to change the
semantics of the program, only to change its efficiency.  This is is what
relational databases do, and it's also what pure functional languages like
Haskell do, in that they add tremendous overhead in the form of tiny functions,
and then brilliantly remove the overhead via optimization.

*(50%)* **Relationalness:** After a NoSQL phase, I becamse quite enamored of
the relational model, and originally TMI was going to be pervasively
relational, but I now think that relational and NoSQL style structures can
coexist nicely.  Still, I firmly believe  that most of the time, if you're not
using a relational database, you're building one.

*(25%)* **Memoization As A Silver Bullet:** I believe there is a sense in which all
optimization can be regarded as eliminating redundant computations of the same
data.  Or as the great Terje Mathisen said, "All programming is an exercise in
caching." I take this to mean that everything non-trivial is really just
caching.  If this is true, then it should be possible to transform (I almost
want to say *rotate*) a program into a form in which it is trivially optimized
by memoization.  Memoization is the most trivial optimization, both to use and
to implement.  Wouldn't it be great if the most trivial optimization was the
only one we needed?  Relational databases do this, with materialized views and
good query optimization.

## Progress

Thus far I have implemented some prototypes containing the first six of the
buzzwords listed above: functional, pure, relational, bidirectional, lens, FRP.

The first prototype is an untyped version implemented in Python.  It started as
a little relational library and suddenly became something else.

The second prototype is a typed version that looks a lot like Haskell (well,
Miranda, really), implemented in Scheme.  It has type checking and inference
and compiles to the lambda calculus which is then compiled to Scheme.

Then I though, hey, I should try this Haskell language I've been reading about
avidly for so long. And I did, and very quickly I was completely hooked.  I see
no reason to implement the core functional part of TMI when I have one that I
find perfect.  So this version of TMI is going to be built in Haskell, and
perhaps on top of Haskell.  It is currently and EDSL.  Later I want to make it
more of its own language, with Template Haskell or even the GHC API.

My current goal, though, is to implement the bare minimum that demonstrates the
fusion of all the buzzwords I listed at the top, and then see if it looks like
it's worth making it a usable tool.

## TODO
- history
- related projects
