<ignore>
  This template will render _authors.tpl with the splices passed to it in
  src/handlers/Conditional.hs. The result will then be inserted into base.tpl.
</ignore>

<apply template="base">
  <apply template="_authors"/>
</apply>
