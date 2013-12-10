## Heist

Heist is a Haskell templating library commonly used with the Snap Framework.
Heist keeps business logic out of the templates by giving you facilities for
manipulating the templates in code. For instance, extracting and repeating
template nodes from code instead of having a special template syntax for looping
and conditions.

## snap-heist-examples

This app is designed to demonstrate a few of the flow control basics common to
other templating systems, such as looping (mapping, in this case) and
conditionally rendering text or conditionally injecting other templates. See
src/handlers/ for standalone Snap handlers demonstrating different ways you
can use Heist.

You can read an intro and the original motivation for it in
[this blog post](http://chromaticleaves.com/posts/the-great-template-heist.html),
or jump straight to the
[compiled Heist walkthrough](http://chromaticleaves.com/posts/compiled-heist-the-walkthrough.html).

## Build steps (using cabal sandboxes)

This was last built with ghc 7.6.3 and cabal 1.18.0. These steps assume you have
those installed at the same major versions (ghc in particular sometimes
introduces breaking changes in new major versions, so no guarantees for this
building easily in future versions).

1. git clone git@github.com:ericrasmussen/snap-heist-examples.git
2. cd snap-heist-examples
3. cabal sandbox init
4. cabal install --only-dependencies
5. (wait patiently)
6. cabal build
7. dist/build/snap-heist-examples/snap-heist-examples -p 3000

The last step will launch the app and server locally on port 3000 (you can use a
different port if you like).

At that point you can interact with it in your browser (usually localhost:3000),
modify the templates and restart the application to see the changes, or change
the code and run cabal build again.

Have fun!
