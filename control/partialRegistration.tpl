<!-- -*-HTML-*- -->
<apply template="outerTemplate">
<script>$(document).ready(function(){setupForm()})</script>
<script>
function setupForm(){
  disableNavigation()
  var qs = $.url().param()
  if(qs.alloc == "True")
    $("a:contains('Allocation')").addClass("disabled")
  if(qs.disburse == "True")
    $("a:contains('Disbursal')").addClass("disabled")
}
</script>
<div class="row">
<div class="span10">
<p> Please perform the following steps to complete your registration.
<div>
<a href="/control/coop/settings" class="btn">Enter Allocation Settings</a>
<a href="/control/coop/settings/disburse/schedule" class="btn">Enter Disbursal Schedule</a>
</div>
</div>
</div>


</apply>
