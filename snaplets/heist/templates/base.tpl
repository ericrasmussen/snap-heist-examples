<!DOCTYPE html>
<!--[if IE 8]>    <html class="no-js lt-ie9" lang="en"> <![endif]-->
<!--[if gt IE 8]><!--> <html class="no-js" lang="en"> <!--<![endif]-->
<head>
  <meta charset="utf-8" />

  <!-- Set the viewport width to device width for mobile -->
  <meta name="viewport" content="width=device-width" />

  <title>Snap Heist Examples</title>

  <link rel="stylesheet" href="/assets/foundation/css/normalize.css">
  <link rel="stylesheet" href="/assets/foundation/css/foundation.css">
  <link rel="stylesheet" href="/assets/foundation-icons/foundation-icons.css">

  <script src="/assets/foundation/js/vendor/custom.modernizr.js"></script>

</head>
<body>

 <nav class="top-bar">
    <ul class="title-area">
      <!-- Title Area -->
      <li class="name">
        <h1>
          <a href="/">
            Snap Heist Examples
          </a>
        </h1>
      </li>
      <li class="toggle-topbar menu-icon"><a href="#"><span>menu</span></a></li>
    </ul>
  </nav>

  <!-- End Top Bar -->


  <!-- Main Page Content and Sidebar -->

  <div class="row">

    <div id="content" class="large-9 columns">
      <apply-content/>
    </div>

    <!-- Sidebar -->
    <div class="large-3 columns">
      <h5>Interpreted Handlers</h5>
      <ul class="side-nav">
        <li>
          <a href="/interpreted/loop">Looping</a>
        </li>
        <li>
          <a href="/interpreted/conditional/text">Conditional Text</a>
        </li>
        <li>
          <a href="/interpreted/conditional/template">Conditional Template</a>
        </li>
      </ul>
      <h5>Compiled Handlers</h5>
      <ul class="side-nav">
        <li>
          <a href="/compiled/loop">Looping</a>
        </li>
        <li>
          <a href="/compiled/conditional/text">Conditional Text</a>
        </li>
        <li>
          <a href="/compiled/conditional/template">Conditional Template</a>
        </li>
        <li>
          <a href="/compiled/conditional/runtime">Conditional Runtime</a>
        </li>
      </ul>
    </div>
    <!-- End Sidebar -->

  </div>

  <!-- End Main Content and Sidebar -->

  <!-- Footer -->

  <footer class="row">
    <div class="large-12 columns">
      <hr />
      <div class="row">
        <div class="large-6 columns">
          <p>&copy; Copyright no one at all. Go to town.</p>
        </div>
      </div>
    </div>
  </footer>

  <!-- End Footer -->

  <script src="/assets/foundation/js/vendor/zepto.js"></script>
  <script src="/assets/foundation/js/foundation.min.js"></script>
  <script>
    $(document).foundation();
  </script>

</body>
</html>

