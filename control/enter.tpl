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
    
    janrain.settings.tokenUrl = 'http://localhost:8000/control/login/resolve/coop';

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
#register {
  border: 1px solid rgb(192,192,192);
  border-radius: 10px 10px 10px 10px;
        margin: 0 auto 20px;
        background-color: rgb(255,255,255);
  height: 156px;
}

</style>

</head>
<body>
<div class="container">
<div class="row">
<div class="span3 offset1" id="register">
  <div style="position:relative; top: 50%; text-align:center">
    <a href="/control/coop/register/authenticate" class="btn">Register Coop</a>
 </div>
</div>
<div class="span5" id="janrainEngageEmbed"></div>
</div>
</div>

<div class="row">
<div class="span8">
  <p>Welcome to the [C]oop [A]ccounting [P]atronage [A]pplication, brought to you by <img src="/img/dsilogo_225x66.gif">
  <p>
  <p>Motivations and Support --here--.
  <p>We intend for this application to be used freely by worker cooperatives. If you find you like this application and are motivated to make a donation, you can learn more about the costs involved in producing and maintaining it, other financials, and a method of donating --here--. 
  <p>Credits: Specifying, Demoing, Development, Systems Admin.
  <p>You can obtain the source code for a local installation --here--. If you seek to significantly modify the system to suit your local installation, or you would like support in setting up a local instance of CAPA, please contact us for availability and consulting rates.
  <p>Technology Platform
  <img src="/img/200px-Platform.svg.png"><img src="/img/80x16_2.gif">
  <img src="/img/happstack.jpg"><img src="/img/poweredby3.png">
  <img src="/img/powered_by_GNU_Emacs.jpeg"><img src="/img/poweredbygnulinux.jpg">
</div>
</div>  
  
</body>
</html>
