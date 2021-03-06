<!DOCTYPE html>
<html>
<head>
<link href="/js/bootstrap/bootstrap.min.css" rel="stylesheet" media="screen"/>
<script src="/js/jquery/jquery.js"></script>
<script src="/js/bootstrap/bootstrap.min.js"></script>
<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-45079819-1', 'camp.coop');
  ga('send', 'pageview');
</script>
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
body {
  padding-top: 10px;
  background-color: #f5f5f5;
}
.container {
  margin: 0 auto;
  max-width: 775px;
}
.container > hr {
  margin: 30px 0;
}
#register {
  border: 1px solid rgb(192,192,192);
  border-radius: 10px 10px 10px 10px;
  margin: 0 auto 20px;
  background-color: rgb(255,255,255);
  height: 156px;
}
#janrainEngageEmbed {
  height: 156px;
}
</style>

</head>
<body>
<div class="container">

<div class="row">
  <div class="span5"><h4 class="muted">Coop Accounting for Member Patronage</h4></div>
  <div class="span2">
  by <a href="http://datasystems.coop"><img width=112 height=33 src="/img/dsilogo_225x66.gif"></a>
  </div>
</div>
<hr>

<div class="row">
<div class="span3 offset1" id="register">
  <div style="position:relative; top: 50%; text-align:center">
    <a href="/control/coop/register/authenticate" class="btn">Register Coop</a>
 </div>
</div>
<div class="span5" id="janrainEngageEmbed"></div>
</div>
<hr>

<div class="row">
<div class="span4">
  <h4>Motives</h4>
  <ul>
   <li>make a small contribution to the worker coop community
   <li>learn deeper about accounting software needs in worker coops
   <li>practice creating and maintaining a software service
   <li>maximize collaboration with other worker coops every step of the way
  </ul>
</div>
<div class="span4">
  <h4>Intentions</h4>
  <ul>
    <li>free to use*
    <li>usable set of basic features
    <li>reliable
    <li>technologically up-to-date implementation*
    <li>open source
    <li>easy to export data, leave the system
    <li>no current plans to add major features**
  </ul>
</div>
</div>

<div class="row">
  <p><small>*into the foreseeable future, contingent on the financial health of Data Systems</small><br>
  <small>**we may elect to grow this project at a later point or implement features users wish to fund</small>
  <dl class="dl-horizontal">
  <dt>Help</dt>
  <dd><small>Initial training <a href='https://docs.google.com/document/d/1Sx--N-3NLLTo-YgIzhlfZsGBKt7k_wlrgc3pf_aSaQg'>documentation</a>. Do not hesitate to email us at support AT camp.coop. </small></dd>
  <dt>Supporting CAMP</dt>
  <dd><small>If you find this application useful, you can <a href='https://www.wepay.com/donations/capa-development-operations-maintenance'>learn more</a> about the costs involved in producing and maintaining it, and a method of donating.</small></dd>
  <dt>Local Installations</dt>
  <dd><small>Obtain the <a href='https://github.com/data-systems-coop/capa'>source code</a>. For support in creating or extensively modifying a local instance of CAMP, please contact Data Systems for rates. </small></dd>
  <dt>Credits</dt>
  <dd><small>Karin Anderson, Kanishka Azimi, Scott Bradley, Erbin Crowell, Aaron Desrochers, Will Desrochers, Roberta Eidman, Yochai Gal, Melissa Hoover, Amy Johnson, Koumbit staff, Mike Leung, Casey Lyons, Jason Mott, Katie Ormiston, Cody Rock, Marc Rudnick, Colin Sagan, Sarah Snider, Jason Storer, Charles Uchu Strader. Special thanks to Erbin for the original idea.</small></dd>
  </dl>
</div>

<div class="row">
  <img width=100 height=35 src="/img/200px-Platform.svg.png">
  <img src="/img/80x16_2.gif">
  <img width=128 height=74 src="/img/happstack.jpg">
  <img width=41 height=62 src="/img/poweredby3.png">
  <img width=57 height=22 src="/img/powered_by_GNU_Emacs.jpeg">
  <img width=40 height=60 src="/img/poweredbygnulinux.jpg">
</div>  

</div>  
</body>
</html>
