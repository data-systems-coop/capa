<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  loadParameters()
  setupForm()
})
</script>

<div class="row">
<div class="span3" id="add">
<script>
function loadParameters(){
  var qs = $.url().param()
  $.getJSON("/member/" + qs.member, function(mem){
    $("#memberName").append(mem.firstName)
  })
  $.getJSON(
     sprintf("/member/equity/account?mbrId=%s&acctId=%s", qs.member, qs.acct), 
     function(acct){
       $("#accountName").append(acct.accountType)
     })
  $("#mbrId").val(qs.member)
  $("#acctId").val(qs.acct)
}
function setupForm(){
  fiscalPeriodPicker("#resultOf")
  $("#resultOf").prepend("<option value=''>None</option>")
  actionTypePicker("#actionType")
  $("#performedOn").datepicker({format:'mm/dd/yyyy', autoclose:true})
  $('form').ajaxForm({  // nice to have use URL to reload account one came from
       success: function(){
         redirect("/control/members/accounts")
       }
    })
  $("#cancel").attr("href","/control/members/accounts")
}
</script>
<form method="POST" action="/member/equity/history"> 

<label>Member</label>
<p id="memberName"></p><input type="hidden" name="mbrId" id="mbrId">
<label>Account</label>
<p id="accountName"></p><input type="hidden" name="acctId" id="acctId">
<label for="actionType">Action Type</label>
<select name='actionType' id='actionType'></select>
<label for="amount">Amount</label>
<input type="text" class="input-mini" name='amount' id="amount">
<label for="performedOn">Performed on</label>
<input type="text" name='performedOn' id="performedOn">
<label for="resultOf">Result of</label>
<select name="resultOf" id="resultOf"></select>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<a id="cancel" class="btn">Cancel</a> 
</div>
</form>

</div>
</div>

</apply>
