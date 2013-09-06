<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  updateNavigation("Member Accounts")
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
    $("#memberName").append(formatMember(mem))
  })
  $.getJSON(
     sprintf("/member/equity/account?mbrId=%s&acctId=%s", qs.member, qs.acct), 
     function(acct){
       $("#accountName").append(acct.accountType)
     })
  $("[name='mbrId']").val(qs.member)
  $("[name='acctId']").val(qs.acct)
}
function setupForm(){
  $("[name='amount']").mask("999000", {reverse:true})
  fiscalPeriodPicker("[name='resultOf']")
  $("[name='resultOf']").prepend("<option value=''>None</option>")
  actionTypePicker("[name='actionType']")
  datePickerDflt("[name='performedOn']")
  $('form').ajaxForm({  // nice to have use URL to reload account one came from
       success: function(){
         redirect("/control/members/accounts")
       }
    })
  $("a:contains('Cancel')").attr("href","/control/members/accounts")
}
</script>
<form method="POST" action="/member/equity/history"> 
<p id="memberName"></p><input type="hidden" name="mbrId">
<p id="accountName"></p><input type="hidden" name="acctId">
<label for="actionType">Action Type</label>
<select name='actionType'></select>
<div class="input-prepend">
  <span class="add-on">$</span>
  <input type="text" class="input-mini" name='amount' placeholder="Amount">
</div>
<input type="text" name='performedOn' placeholder="Performed on" class="input-small">
<label for="resultOf">Result of</label>
<select name="resultOf"></select>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button> <a class="btn">Cancel</a> 
</div>
</form>

</div>
</div>

</apply>
