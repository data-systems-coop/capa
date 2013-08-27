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
  <p>A few people have asked us what our motivations and intentions are for this project. Our motivations are to make a small contribution to the worker coop community and to use this as a way to learn deeper about accounting software needs in worker coops. Also, we used it as a training ground for understanding the various tasks involved in establishing a software service. We intend to provide a usable set of basic features and try to ensure that the service is stable and reliable. We would like to support the hosting of this software service for free into the forseeable future, as well as perform basic upkeep to the implementation to keep it technologically current. This is contingent on the financial health of Data Systems. We are also committed to keeping the implementation open source. We have incorporated a feature to easily export all of your data, should you decide to leave the system.
  <p>We intend for this application to be used freely by worker cooperatives. If you find you like this application and are motivated to make a donation, you can learn more about the costs involved in producing and maintaining it, and a method of donating <a href='https://www.wepay.com/donations/capa-development-operations-maintenance'>here</a>.
  <p>Credits: CAPA Advisory Board(MH,...), Amy, Developers/team initial version(Katie, Jason, ...), Demoing(Alvarado, A Bookkeeping Coop, Roberta), Development(Aaron,Kanishka,), Systems Admin(Casey,..).
  <p>You can obtain the source code for a local installation <a href='https://github.com/data-systems-coop/capa'>here</a>. If you seek to significantly modify the system to suit your local installation, or you would like support in setting up a local instance of CAPA, please contact us for availability and rates. Also, once we have settled on a useful set of basic features, we do not intend to add any major features unless we decide to make this a priority for a future contribution or users approaches us to fund adding a feature.
  <p>Technology Platform
  <img src="/img/200px-Platform.svg.png"><img src="/img/80x16_2.gif">
  <img src="/img/happstack.jpg"><img src="/img/poweredby3.png">
  <img src="/img/powered_by_GNU_Emacs.jpeg"><img src="/img/poweredbygnulinux.jpg">
</div>
</div>  
  
</body>
</html>
