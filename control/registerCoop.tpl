<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  disableNavigation()
  loadParameters()
  setupForm()
})
</script>

<div class="row">
<div class="span3" id="add">
<script>
function loadParameters(){
  var qs = $.url().param()
  $("#username").append(qs.username)
  $("[name='username']").val(qs.username)
}
function setupForm(){
  monthPicker("[name='clTpStart']")
  periodTypePicker("[name='clTpPeriodType']")
  $('form').ajaxForm({
       success: function(){
         window.location.href = "/control/coop/summary"
       }
    })
}
function periodTypePicker(id){
  ['Year','Quarter'].forEach(function(t){
    $(id).append(sprintf("<option value='%s'>%s</option>", t, t))
  })
}
</script>
<form method="POST" action="/coop"> 
<label for="cpName">Name</label>
<input type="text" name='cpName'>
<label>Username</label>
<p id="username"></p><input type="hidden" name="username">
<label for="clTpPeriodType">Fiscal Period Type</label>
<select name='clTpPeriodType'></select>
<label for="clTpStart">Fiscal Year Start Month</label>
<select name='clTpStart'></select>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<a href="/control/enter" class="btn">Cancel</a> 
</div>
</form>

</div>
</div>

</apply>
