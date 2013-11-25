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

