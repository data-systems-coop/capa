<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  disableNavigation()
  setupForm()
})
</script>

<script>
function setupForm(){
  loadDisbursalSchedule([[{"years":1,"months":2},0.6]])
  setupAddDisbursal()
  $('form').ajaxForm({
       success: function(){
	 redirect("/control/coop/summary")
       }
    })
}
</script>
<form method="POST" action="/coop/settings/disburse/schedule/default">
<script>
function formatDisbursal(d){
  return sprintf("<td>%s years %s months</td><td>%s %%</td>", 
           d[0].years, d[0].months , d[1]*100)
}
function loadDisbursalSchedule(s){
  $("#disbursalScheduleRows").data("d", s)
  saveToHidden()
  $("#disbursalScheduleRows").empty()
  s.forEach(function(d){
    $("#disbursalScheduleRows").append(
       sprintf("<tr>%s<td>X</td></tr>", formatDisbursal(d)))
  })
}
function saveToHidden(){
  $("#disbursalSchedule").val(JSON.stringify($("#disbursalScheduleRows").data("d")))
}
</script>
<div class="row">
  <div class="span3">
	  <label>Default Disbursal Schedule</label>
  </div>
</div>
<div class="row">
    <div class="span4">	  
	  <input type="hidden" name="disbursalSchedule" id="disbursalSchedule">
	  <table class="table">
	    <thead>
	      <tr><th>After Allocation</th><th>Proportion</th><th></th></tr>
	    </thead>
 	    <tbody id="disbursalScheduleRows"></tbody>
	  </table>
	  <button type="button" class="btn btn-small" data-toggle="modal" data-target="#addModal">Add Disbursal</button>
    </div>
</div>	  

<div class="row">
    <div class="span5">
	  <div class="form-actions">
	    <button type="submit" class="btn btn-primary">Save</button>
	    <a class="btn" href="/control/coop/summary">Cancel</a> 
	  </div>
    </div>
</div>
</form>


<script>
function yearCountPicker(id){
  var years = [0,1,2,3,4,5,6,7]
  years.forEach(function(y){
    $(id).append(sprintf("<option>%s</option>", y))
  })
}
function monthCountPicker(id){
  [0,1,2,3,4,5,6,7,8,9,10,11].forEach(
    function(m){
      $(id).append(sprintf("<option>%s</option>",m))
    })
}
function setupAddDisbursal(){
  yearCountPicker("#years")
  monthCountPicker("#months")
  $("#addButton").click(function(){
    var disb = 
      [[{years: parseInt($("#years").val()), months: parseInt($("#months").val())}, 
       $("#proportion").val() / 100]]
    var arr = $("#disbursalScheduleRows").data("d")
    loadDisbursalSchedule(arr.concat(disb))
    $("#addModal").modal('hide')
  })
}
</script>	
<div id="addModal" class="modal hide fade" tabindex="-1" 
	     role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-header">
    <button type="button" class="close" data-dismiss="modal" aria-hidden="true">x</button>
    <h3 id="myModalLabel">Add Disbursal</h3>
  </div>
  <div class="modal-body">
      <label for="years">Years after allocation</label>
      <select id="years"></select>
      <label for="months">Months after allocation</label>
      <select id="months"></select>
      <label for="months">Proportion (%)</label>
      <input type="text" id="proportion" name="proportion">
  </div>
  <div class="modal-footer">
    <button class="btn" data-dismiss="modal" aria-hidden="true">Cancel</button>
    <button type="button" id="addButton" class="btn btn-primary">Add</button>
  </div>
</div>



</apply>
