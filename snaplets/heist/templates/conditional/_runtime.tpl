<h2>Compiled Runtime Values</h2>

<p>
  This template has two tags, runtimeA and runtimeB. In
  src/handlers/ConditionalCompiled.hs, we create a splice that inspects these
  runtime values and chooses which template to apply (either just_value.tpl or
  nothing.tpl).
</p>

<p>
  There is no corresponding interpreted splice, because interpreted splices make
  it easy to work with the values directly. You can look at
  src/handlers/Conditional.hs for an example of calling templates based on
  values.
</p>

<!-- we expect a value here -->

<h4>The runtime value is Just "a value"</h4>
<hr />

  <runtimeA/>

<!-- we expect nothing here -->

<h4>The runtime value is Nothing</h4>
<hr />

  <runtimeB/>

