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
    var rollingAcct = 
      accts.filter(function(a){return a.accountType == 'RollingPatronageAcct'})[0]
    var buyinAcct = 
      accts.filter(function(a){return a.accountType == 'BuyInAcct'})[0]
    reloadActions(
      $("[name='member']").val(), rollingAcct.ida, "#rollingResult",
      "#rollingAdd")
    reloadActions(
      $("[name='member']").val(), buyinAcct.ida, "#buyinResult",
      "#buyinAdd")
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
    reselectMember()
    //$("[name='member']").change()
  })
}
function reselectMember(){
  var mbrId = $.url().param().mbrId
  if( mbrId != undefined )
    $("[name='member']").val(mbrId)
  $("[name='member']").change()
}
</script>

<form method="GET" action="/control/member/account/action/add">
<div class="row"><div class="span3"><select name="member"></select></div></div>
<input type="hidden" name="acct"/>
<!-- <div class="row"><div class="span4"><select name="acct"></select></div></div> -->

<script>
function reloadActions(mbrId, acctId, target, button){
  $(target).empty()
  $.getJSON(
     sprintf("/member/equity/account/actions?mbrId=%s&acctId=%s",mbrId,acctId),
     function(acns){
       $.each(acns, function(i,a){ loadAction(a, target) })
     })
  $(button).click(function(){
     $("[name='acct']").val(acctId)
  })
}
function loadAction(a, target){
  var act = a[0]
  var res = a[1]
  var resFmtd = (res == null) ? "" : formatFiscalPeriod(res)
  var bal = a[2]
  $(target).append(
    sprintf("<tr><td>%s</td><td>%s</td><td>$%s</td><td>%s</td><td>$%s</td></tr>", 
       formatGregorianDay(act.performedOn), act.actionType, amountFormat(act), 
       resFmtd, bal))
}
</script>
<div class="row">
<div class="span7">
<h4>Rolling Patronage Account</h4>
<table class="table">
<thead>
  <tr><th>Performed on</th><th>Type</th><th>Amount</th>
      <th>Result of</th><th>Balance</th></tr>
</thead>
<tbody id="rollingResult"></tbody>
</table>

</div>
</div>

<div class="row">
<div class="span2">
<button id="rollingAdd" type="submit" class="btn btn-primary">Add Action</button>
</div>
</div>

<div class="row"><div class="span7"><hr></div></div>


<div class="row">
<div class="span7">
<h4>Buy In Acount</h4>
<table class="table">
<thead>
  <tr><th>Performed on</th><th>Type</th><th>Amount</th>
      <th>Result of</th><th>Balance</th></tr>
</thead>
<tbody id="buyinResult"></tbody>
</table>

</div>
</div>

<div class="row">
<div class="span2">
<button id="buyinAdd" type="submit" class="btn btn-primary">Add Action</button>
</div>
</div>

</form>

</apply>
