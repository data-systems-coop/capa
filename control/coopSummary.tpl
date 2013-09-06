<!-- -*-HTML-*- -->
<apply template="outerTemplate">
<script>$(document).ready(function(){ setupForm() })</script>
<script>
function setupForm(){
  updateNavigation('Home')
  loadMemberSummary()  
  loadCoop()
}
</script>

<script>
function loadCoop(){
  $.getJSON("/coop", function(c){
    $("#name").append(c.name)
    $("#usageDates").append(sprintf("%s/%s", c.usageStart[1], c.usageStart[0]))
  })
}
</script>
<div class="row">
<div class="span5">
<h3><span id="name"/> [<span id="usageDates"/>-Present]</h3>
</div>
</div>


<script>
function loadMemberSummary(){
  $("#now").append((new Date()).toLocaleDateString())
  $.getJSON("/members", function(ms){ $.each(ms, function(i,m){ loadMember(m) }) })
}
function loadMember(m){
  var member = m[0]
  var bal = m[1]
  $("#memberSummary").append(
    sprintf("<tr><td>%s</td><td>$%s</td><td>%s</td></tr>",
             formatMember(member), bal, 
             formatGregorianDay(member.acceptedOn)))
}
</script>
<div class="row">
<div class="span8">

<table class="table">
<thead><tr><th>Member</th><th>Equity(<span id="now"/>)</th><th>Accepted on</th></tr></thead>
<tbody id="memberSummary"></tbody>
</table>

</div>
</div>


<div class="row">
<div class="span3">
<a class="btn" href="/control/members/add">New Member</a>
</div>
</div>

</apply>
