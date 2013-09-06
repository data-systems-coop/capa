<!-- -*-HTML-*- -->
<apply template="outerTemplate">
<script>
$(document).ready(function(){
  updateNavigation("Financial Results")
  setupForm()
})
function setupForm(){
  $('form').ajaxForm({
     success: function(){
       window.location.href="/control/financial/results"
     }
  })
  fiscalPeriodPicker("[name='over']")
  $("[name='surplus']").mask("999000", {reverse:true})
}
</script>
<div class="row">
<div class="span3">

<form method="POST" action="/financial/results"> 
<label for="over">Over</label>
<select class="input-medium" name='over'></select>
<div class="input-prepend">
  <span class="add-on">$</span>
  <input type="text" class="input-small" name='surplus' placeholder='Surplus'>
</div>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Record</button>
<a class="btn" href="/control/financial/results">Cancel</a>
</div>
</form>

</div>
</div>

</apply>
