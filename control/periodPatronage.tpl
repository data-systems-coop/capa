<apply template="outerTemplate">

<div class="row">
<div class="span3">
<select>
  <option value="">FY 2012</option>
  <option value="">FY 2011</option>
</select>
</div>
</div>

<script>
$(document).ready(function() {
  updateNavigation("Patronage")
  loadMembers()          
  cancelAddMode()
  setupAddForm()
})
</script>

<div class="row">
<div class="span8">
<script>
function attachAddHandlers(){
  $("tr").click(function() {
    enableAddMode(this, $(this).attr("id"))
  })
}
function loadMembers(){
  $.getJSON("/members", function(data){
     $("#members").data("members", data)
     $.each(data, function(i,m){
	appendMember(m)
     })
     attachAddHandlers()
  })
}
function loadPatronage(memberId){
  $.getJSON(sprintf("/member/%s/patronage/1",memberId), function(data){
    updatePatronage(memberId, data)
  })
}
function appendMember(member){
  $("#members").append(
     sprintf("<tr id='%s'><td colspan=6>%s</td></tr>", 
              member.firstName, member.firstName))
}
function updatePatronage(memberId, patronage){
  var row = $("#" + memberId)
  row.empty()
  row.append(
        sprintf("<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td>", 
                memberId, patronage.work, patronage.skillWeightedWork, 
                patronage.seniority, patronage.quality, patronage.revenueGenerated))
}
</script>
<table class="table table-hover">
<thead>
  <tr><th>Name</th><th>Productive Hours</th><th>Salary</th></tr>
</thead>
<tbody id="members">
</tbody>
</table>

</div>



<div class="span3" id="add">
<script>
function cancelAddMode(){
  $("#add").hide()
  $("tr").removeClass("info")
}
function enableAddMode(row, memberId){
  cancelAddMode()
  $(row).addClass("info")
  $("#addForm input").val("")
  $("#add").show()
  $("#addForm").attr("action",sprintf("/member/%s/patronage/1", memberId))
  console.log($("#performedOver").val())
}
function setupAddForm(){
  $("#cancel").click(cancelAddMode)
  $('#addForm').ajaxForm({
       success: function(){
         loadPatronage($(".info").attr("id"))
         cancelAddMode()
       }
    })
  //fill the select
}
//fill fiscal periods
   //fetch fiscal periods
</script>
<form id="addForm" method="POST"> 
<label for="work">Work</label>
<input type="text" class="input-mini" name='work' id="work">
<label for="skillWeightedWork">Skill-Weighted Work</label>
<input type="text" class="input-mini" name='skillWeightedWork' id="skillWeightedWork">
<label for="seniority">Seniority</label>
<input type="text" class="input-mini" name='seniority' id="seniority">
<label for="quality">Quality</label>
<input type="text" class="input-mini" name='quality' id="quality">
<label for="revenueGenerated">Revenue Generated</label>
<input type="text" class="input-mini" name='revenueGenerated' id="revenueGenerated">
<label for="performedOver">Performed Over</label>
<select class="input-small" name='performedOver' id="performedOver" disabled>
  <option value='{"start":{"year":2013,"month":1},"periodType":"Year"}'>2013</option>  
</select>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<button id="cancel" type="button" class="btn">Cancel</button>
</div>
</form>

</div>

</div>

</apply>