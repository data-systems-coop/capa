<apply template="outerTemplate">

<script>
$(document).ready(function() {
  setupAddForm()
})
</script>

<div class="row">
<div class="span3" id="add">
<script>
function setupAddForm(){
  $("#addForm").attr("action",sprintf("/member/%s/patronage/1", memberId))
  $("#cancel").click(cancelAddMode)
  $('#addForm').ajaxForm({
       success: function(){
         loadPatronage($(".info").attr("id"))
         cancelAddMode()
       }
    })
  //fill the select
}
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
<select class="input-medium" name='performedOver' id="performedOver" disabled>
</select>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<button id="cancel" type="button" class="btn">Cancel</button>
</div>
</form>

</div>
</div>

</apply>
