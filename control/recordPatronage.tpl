<apply template="outerTemplate">

<script>
$(document).ready(function() {
  loadParameters()
  setupForm()
})
</script>

<div class="row">
<div class="span3" id="add">
<script>
function loadParameters(){
  var qs = $.url().param()
  $.getJSON("/member/" + qs.memberId, function(mem){
    $("#memberName").append(mem.firstName)
    $("#performedOver").append(formatFiscalPeriod(JSON.parse(qs.period)))
    $("#addForm").attr("action",sprintf("/member/%s/patronage/%s", qs.memberId, qs.period))
  })
}
function setupForm(){
  seniorityPicker("#seniority")
  qualityPicker("#quality")  
  //get method -> name. use name to get field list
  var allFields =       
      ["work", "skillWeightedWork", "seniority", "quality", "revenueGenerated"]
  var calcMethodFields = 
      ["work", "skillWeightedWork", "seniority"]
  allFields.forEach(function(e){
    if(calcMethodFields.indexOf(e) == -1){
      $("#" + e).remove()
      $("label[for='" + e + "']").remove()
    }
  })
  $('#addForm').ajaxForm({
       success: function(){
         var per = uriEncode($.url().param("period"))
         window.location.href = sprintf("/control/members/patronage/period?period=%s", per)
       }
    })
  var per = uriEncode($.url().param("period"))
  $("#cancel").attr("href",sprintf("/control/members/patronage/period?period=%s",per))
}
</script>
<form id="addForm" method="POST"> 

<label>Member</label>
<p id="memberName"></p>
<label>Performed Over</label>
<p id="performedOver"></p>
<label for="work">Work</label>
<input type="text" class="input-mini" name='work' id="work">
<label for="skillWeightedWork">Skill-Weighted Work</label>
<input type="text" class="input-mini" name='skillWeightedWork' id="skillWeightedWork">
<label for="seniority">Seniority</label>
<select class="input-small" name='seniority' id="seniority"></select>
<label for="quality">Quality</label>
<select class="input-small" name='quality' id="quality"></select>
<label for="revenueGenerated">Revenue Generated</label>
<input type="text" class="input-mini" name='revenueGenerated' id="revenueGenerated">
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<a id="cancel" class="btn">Cancel</a> 
</div>
</form>

</div>
</div>

</apply>
