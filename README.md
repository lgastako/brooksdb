# brooksdb

This is an attempt to implement a relational database management system based
primarily on the ideas of Date & Darwen's Third Manifesto and the Haskell
programming language.

In addition it serves as a playground for some of my ideas.  An example of which is
that I believe that databases should be very strongly typed, in the vein of Haskell
itself, but also that relational databases should start out much more malleably
so that they allow for better prototyping and exploratory work.  To that end I
intend to implement some other data types in the database alongside relvar's that
are easier to work with, bags, sets and other similar structures that can be operated
on in the same transactional way as real relvars.

"Brooks" is Haskell Curry's middle name.

## Building / Installing

Once, to set everything up:

    make setup

Build:

    make build

Test:

    make test

Demo:

    make build demo

## TODOs

- implement client/server model using Data.Acid.Remote
- provide a mechanism for specifying types in loadrel
  - maybe just require a second header row?
