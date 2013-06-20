<apply template="outerTemplate">

<script>
$(document).ready(function() { 
  updateNavigation("Allocate")
  setupAllocateForm()
  loadMembers()
})
</script>

<div class="row">
<div class="span2 offset3">
<script>
function setupAllocateForm(){
  $('#allocateForm').ajaxForm({
     dataType: "json",
     success: attachAllocations
   })
}
</script>
<form id="allocateForm" method="POST" action="/equity/members/allocate">
<label for="surplus">Surplus</label>
<input type="text" class="input-small" name="surplus" id="surplus"/>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Allocate</button>
</div>
</form>
</div>
</div>


<div class="row">

<div class="span10">
<script>
function loadMembers(){
  $.getJSON("/members", function(data){
     $("#result").data("members", data)
     $.each(data, function(i,m){ loadPatronage(m) })
  })
}
function loadPatronage(member){
  $.getJSON(sprintf("/member/%s/patronage/1", member.firstName), function(pdata){
     appendMember(member, pdata)
   })
}
function appendMember(member, patronage){
  $("#result").append(
    sprintf(
 "<tr id='%s'><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td></td></tr>", 
        member.firstName, member.firstName, 
        patronage.work, patronage.skillWeightedWork, patronage.seniority, 
        patronage.quality, patronage.revenueGenerated))
}
function attachAllocations(allocations){
  $("#result").data("allocations", allocations)
  $.each(allocations, function(i,a){ attachAllocation(a[0],a[1]) })
}
function attachAllocation(member, equityAction){
  $("#" + member.firstName + " td:nth-child(7)").remove()
  $("#" + member.firstName).append(sprintf("<td>%s</td>",equityAction.amount))
}
</script>
<table class="table">
<thead>
  <tr><th>Name</th><th>Work</th><th>Skill-Weighted Work</th>
  <th>Seniority</th><th>Quality</th><th>Revenue Generated</th><th>Allocated</th></tr>
</thead>
<tbody id="result">
</tbody>
</table>
</div>

</div>

</apply>
