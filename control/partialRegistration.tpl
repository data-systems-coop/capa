<!DOCTYPE html>
<html>
<head>
<link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet" media="screen"/>
<script src="//code.jquery.com/jquery.js"></script>
<script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"></script>
<script src="//cdnjs.cloudflare.com/ajax/libs/jquery-url-parser/2.3.1/purl.min.js"></script>
<script>$(document).ready(function(){setupForm()})</script>
</head>
<body>
<script>
function setupForm(){
  var qs = $.url().param()
  if(qs.alloc == "True")
    $("a:contains('Allocation')").addClass("disabled")
  if(qs.disburse == "True")
    $("a:contains('Disbursal')").addClass("disabled")
}
</script>
<div class="container">
<div class="row">
<p> Please perform the following steps to complete your registration.
<div><a href="/control/coop/settings" class="btn">Enter Allocation Settings</a>
<a href="/control/coop/settings/disburse/schedule" class="btn">Enter Disbursal Schedule</a>
</div>
</div>
</div>
</body>
</html>
