<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() { 
  var over = JSON.parse($.url().param("over"))
  loadAllocationsDisbursals(over)
  setupSaveForm(over);
})
</script>

<div class="row">
<div class="span10">
<script>
function loadAllocationsDisbursals(over){
  var allocUrl = "/equity/members/allocate/generate"
  $.post(allocUrl, {"over": JSON.stringify(over)}, function(data){
      $.each(data, function(i,d){
       var memId = d[0].memberId
       appendMemberAllocation(d[0], d[1])
       loadDisbursals(d[0], d[1], sprintf("#m%s", memId))  
     })
   })
}
function appendMemberAllocation(member, allocAction){
  $("#result").append(
    sprintf("<tr id='m%s'><td>%s</td><td>%s</td><td>%s</td></tr>", 
      member.memberId, member.firstName, allocAction.amount, allocAction.actionType))
}
function loadDisbursals(member, alloc, containerId){
  $.post("/member/equity/disburse", 
         {"allocateAction": JSON.stringify(alloc)}, function(disburses){
     var subTableRows = 
       $.map(disburses, function(d){
  	  return sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", 
	  	  d.amount, d.actionType, formatGregorianDay(d.performedOn))
        }).join(" ")
     $(containerId).append(
       sprintf("<td><table><thead>" + 
               "<tr><th>Amount</th><th>Action Type</th><th>Performed On</th></thead>" + 
	       "<tbody>%s</tbody></table></td>", 
           subTableRows))
  })
}
function setupSaveForm(over){
  $('#over').val(JSON.stringify(over))
  $('#saveForm').ajaxForm({
      success: function(){
        window.location.href = sprintf("/control/financial/results")
      }
  })
}
</script>

<table class="table">
<thead>
<tr><th>Name</th><th>Allocated</th><th>Action Type</th><th>Disbursals</th></tr>
</thead>
<tbody id="result"></tbody>
</table>
</div>
</div>

<div class="row">
<div class="span3">
<form id="saveForm" method="POST" action="/equity/members/allocate/save">
<input type="hidden" name="over" id="over">
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<a href="/control/financial/results" id="cancel" class="btn">Cancel</a>
</div>
</form>
</div>
</div>

</apply>
