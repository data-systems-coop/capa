<!-- -*-HTML-*- -->
<apply template="outerTemplate">
<script>$(document).ready(function(){
  updateNavigation("Member Accounts") 
  setupForm() })</script>
<script>
function setupForm(){
  var memAcctMap = new Object()
  $("[name='member']").change(function(){
    var sel = $("[name='member']").val()
    var accts = memAcctMap[sel]
    $("[name='acct']").empty()
    accts.forEach(function(a){
      $("[name='acct']").append(sprintf("<option value='%s'>%s</option>", a.ida, a.accountType))
    })
    $("[name='acct']").change()
  })
  $("[name='acct']").change(function(){
    reloadActions($("[name='member']").val(), $("[name='acct']").val())
  })
  $.getJSON("/members/equity/accounts", function(memAccts){
    memAccts.forEach(function(el){
      var mbrId = el[0].memberId
      var accts = el[1]
      memAcctMap[mbrId] = accts
    })
    var members = $.map(memAccts, function(el){ return el[0] })
    members.forEach(function(m){
         $("[name='member']").append(
            sprintf("<option value='%s'>%s</option>", m.memberId, formatMember(m)))})
    $("[name='member']").change()
  })
}
</script>

<form method="GET" action="/control/member/account/action/add">
<div class="row"><div class="span3"><select name="member"></select></div></div>
<div class="row"><div class="span4"><select name="acct"></select></div></div>

<script>
function reloadActions(mbrId, acctId){
  $("#result").empty()
  $.getJSON(
     sprintf("/member/equity/account/actions?mbrId=%s&acctId=%s",mbrId,acctId),
     function(acns){
       $.each(acns, function(i,a){ loadAction(a) })
     })
}
function loadAction(a){
  var act = a[0]
  var res = a[1]
  var resFmtd = (res == null) ? "" : formatFiscalPeriod(res)
  var bal = a[2]
  $("#result").append(
    sprintf("<tr><td>%s</td><td>%s</td><td>$%s</td><td>%s</td><td>$%s</td></tr>", 
       formatGregorianDay(act.performedOn), act.actionType, act.amount, 
       resFmtd, bal))
}
</script>
<div class="row">
<div class="span7">

<table class="table">
<thead>
  <tr><th>Performed on</th><th>Type</th><th>Amount</th>
      <th>Result of</th><th>Balance</th></tr>
</thead>
<tbody id="result"></tbody>
</table>

</div>
</div>

<div class="row">
<div class="span2"><button type="submit" class="btn btn-primary">Add Action</button></div>
</div>
</form>

</apply>
