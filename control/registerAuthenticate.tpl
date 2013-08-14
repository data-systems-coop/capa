<!DOCTYPE html>
<html>
<head>
<link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet" media="screen"/>
<script src="//code.jquery.com/jquery.js"></script>
<script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"></script>
<script src="https://raw.github.com/alexei/sprintf.js/master/src/sprintf.min.js"></script>
<script>
(function() {
    if (typeof window.janrain !== 'object') window.janrain = {};
    if (typeof window.janrain.settings !== 'object') window.janrain.settings = {};
    
    janrain.settings.tokenUrl = 'http://localhost:8000/control/login/register';

    function isReady() { janrain.ready = true; };
    if (document.addEventListener) {
      document.addEventListener("DOMContentLoaded", isReady, false);
    } else {
      window.attachEvent('onload', isReady);
    }

    var e = document.createElement('script');
    e.type = 'text/javascript';
    e.id = 'janrainAuthWidget';

    if (document.location.protocol === 'https:') {
      e.src = 'https://rpxnow.com/js/lib/capa/engage.js';
    } else {
      e.src = 'http://widget-cdn.rpxnow.com/js/lib/capa/engage.js';
    }

    var s = document.getElementsByTagName('script')[0];
    s.parentNode.insertBefore(e, s);
})();
</script>
<style type="text/css">
.body {
  padding-top: 100px;
  background-color: #f5f5f5;
}
</style>

</head>
<body>
<div class="container">
<div class="row">
<div class="span5" id="janrainEngageEmbed"></div>
</div>
</div>

</body>
</html>
