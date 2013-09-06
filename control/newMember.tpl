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
<form method="POST" action="/member"> 

<div class="row">
<div class="span6">
<input type="text" name='firstName' placeholder="First Name">
<input type="text" name='lastName' placeholder="Last Name">
</div>
</div>

<div class="row">
<div class="span6">
<input type="text" class="input-small" name='acceptedOn' placeholder="Accepted on">
<div class="form-actions">
<button type="submit" class="btn btn-primary">Add</button>
<a class="btn" href="/control/coop/summary">Cancel</a>
</div>
</div>
</div>

</form>
</apply>
