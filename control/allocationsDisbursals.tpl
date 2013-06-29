<apply template="outerTemplate">

<script>
$(document).ready(function() { 
  var over = JSON.parse($.url().param("over"))
  loadAllocationsDisbursals(over)
  setupSaveForm();
})
</script>

<div class="row">
<div class="span10">
<script>
function loadAllocationsDisbursals(over){
  var allocUrl = "/equity/members/allocate"
  $.post(allocUrl, {"over": JSON.stringify(over)}, function(data){
      $.each(data, function(i,d){
       appendMemberAllocation(d[0], d[1])
       loadDisbursals(d[1])  
     })
   })
}
function appendMemberAllocation(member, allocAction){
  $("#result").append(
    sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", 
      member.firstName, allocAction.amount, allocAction.actionType))
}
function loadDisbursals(alloc){
  //get all disbursals
  //create disbursal container
  // each
  //   appendDisbursal
}
function setupSaveForm(){
  //set form action
  //ajax form
  //   on success -> results
}
</script>

<table class="table">
<thead>
  <tr><th>Name</th><th>Allocated</th><th>Action Type</th></tr>
</thead>
<tbody id="result">
</tbody>
</table>
</div>
</div>

<div class="row">
<div class="span3">
<form id="saveForm" method="POST">
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<a href="/control/financial/results" id="cancel" class="btn">Cancel</a>
</div>
</form>
</div>
</div>

</apply>
