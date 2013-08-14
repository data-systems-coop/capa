<!-- -*-HTML-*- -->
<apply template="outerTemplate">
<script>$(document).ready(function(){ 
  setupForm() 
})</script>
<script>
function setupForm(){
  datePickerDflt("[name='acceptedOn']")
  $('form').ajaxForm({
    success: function(){ redirect("/control/coop/summary") }
  })
}
</script>
<div class="row">
<div class="span3">

<form method="POST" action="/member"> 
<label for="firstName">First Name</label>
<input type="text" class="input-small" name='firstName'>
<label for="lastName">Last Name</label>
<input type="text" class="input-small" name='lastName'>
<label for="acceptedOn">Accepted on</label>
<input type="text" class="input-small" name='acceptedOn'>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Add</button>
<a class="btn" href="/control/coop/summary">Cancel</a>
</div>
</form>

</div>
</div>

</apply>
