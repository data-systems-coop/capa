<apply template="outerTemplate">

<script>
$(document).ready(setupForm)
function setupForm(){
  $("#recordResult").ajaxForm({
     success: function(){
       window.location.href="/control/financial/results"
     }
  })
  fiscalPeriodPicker("#over")
}
function fiscalPeriodPicker(id){
  $.getJSON("/fiscal/periods",function(periods){
    $.each(periods,function(i,period){
      addPeriod(period, id)
    })
  })
}
function addPeriod(per, id){ 
  var opt = 
    sprintf("<option value='%s'>%s</option>", 
            JSON.stringify(per), 
            formatFiscalPeriod(per))
  $(id).append(opt)
}
</script>

<div class="row">
<div class="span3">

<form id="recordResult" method="POST" action="/financial/results"> 
<label for="over">Over</label>
<select class="input-medium" name='over' id="over"></select>
<label for="surplus">Surplus</label>
<input type="text" class="input-small" name='surplus' id="surplus">
<div class="form-actions">
<button type="submit" class="btn btn-primary">Record</button>
<button id="cancel" type="button" class="btn">Cancel</button>
</div>
</form>

</div>
</div>

</apply>
