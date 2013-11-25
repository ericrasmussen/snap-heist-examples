<h2>Conditional Text</h2>

<p>
  The two authors in this table are inserted one by one in
  src/handlers/Conditional.hs to show how the author text can be inserted
  conditionally depending on whether or not we have an author name.
</p>

<table>
  <thead>
    <tr>
      <th>Title</th>
      <th>Author</th>
    </tr>
  </thead>
  <tbody>


  <ignore>
    The following node demonstrates text chosen conditionally in code.
    See: src/handlers/Conditional.hs for details.

    Note: in order to avoid complicating this code with loops or other types of
    control flow, we laboriously pass in each of the two tutorial types one by
    one. For examples of looping, see src/Loop.hs or src/LoopConditional.hs.
  </ignore>

  <conditionalText>
    <tr>
      <td>
        <a href="${urlA}"><titleA/></a>
      </td>
      <td>
        <maybeAuthorA/>
      </td>
    </tr>

    <tr>
      <td>
        <a href="${urlB}"><titleB/></a>
      </td>
      <td>
        <maybeAuthorB/>
      </td>
    </tr>
  </conditionalText>

  </tbody>
</table>
