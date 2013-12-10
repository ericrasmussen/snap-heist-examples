<ignore>
  This template will render _runtime.tpl with the splices passed to it in
  src/handlers/ConditionalCompiled.hs. The result will then be inserted into base.tpl.
</ignore>

<apply template="base">
  <apply template="_runtime"/>
</apply>
