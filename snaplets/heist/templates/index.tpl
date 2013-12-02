<ignore>
  When called, this template will be inserted in base.tpl.
</ignore>

<apply template="base">

<h4>Examples Overview</h4>

<p>
  Heist is a Haskell templating library commonly used with the Snap Framework.
  Heist keeps business logic out of the templates by giving you facilities for
  manipulating the templates in code. For instance, extracting and repeating
  template nodes from code instead of having a special template syntax for
  looping and conditions.
</p>

<p>
  This site is designed to demonstrate a few of the flow control basics common
  to other templating systems, such as looping (mapping, in this case) and
  conditionally rendering text or conditionally injecting other templates.
</p>

<p>
  On the right sidebar you'll see interpreted and compiled Heist examples. You
  can view the code for them
  <a href="https://github.com/ericrasmussen/snap-heist-examples/tree/master/src/handlers">
    here</a>. An effort has been made to make the interpreted and compiled code
  similar, with lots of comments to try to explain the conceptual differences.
  Interpreted splices are arguably easier to learn and use, but they are much,
  much slower. I recommend starting with Compiled splices if possible, learning
  both if not, and comparing the two sets of examples if you ever need to port
  one to the other.
</p>

<p>
  You can read more on the official <a href="http://snapframework.com/">Snap
  Site</a>.
</p>

</apply>
