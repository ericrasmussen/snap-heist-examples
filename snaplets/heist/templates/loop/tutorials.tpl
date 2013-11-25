<ignore>
  When called, this template will call _tutorials.tpl with any bound splices,
  render it, and insert the result in base.tpl.
</ignore>

<apply template="base">
  <apply template="_tutorials"/>
</apply>
