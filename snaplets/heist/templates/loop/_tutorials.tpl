<h2>Looping</h2>

<p>
  This table is rendered by mapping over a list of Tutorials to create a list of
  splices. See src/handlers/Loop.hs for details.
</p>

<table>
  <thead>
    <tr>
      <th>Title</th>
      <th>Author</th>
    </tr>
  </thead>
  <tbody>


  <allTutorials>

    <tr>
      <td>
        <a href="${tutorialURL}"><tutorialTitle/></a>
      </td>
      <td>
        <tutorialAuthor/>
      </td>
    </tr>

  </allTutorials>

  </tbody>
</table>
