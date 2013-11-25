<h2>Conditional Template</h2>

<p>
  This template has two tags, authorA and authorB. In
  src/handlers/Conditional.hs, we look at two different authors (Maybe Text) and
  fill in the rendered "authorinfo" template only in the Just case.
</p>

<!-- we expect nothing to render for the first author -->

<h4>authorA (Nothing)</h4>
<hr />

  <authorA/>

<!-- we expect a rendered template to be inserted here -->

<h4>authorB (Just "mightybyte")</h4>
<hr />

  <authorB/>

