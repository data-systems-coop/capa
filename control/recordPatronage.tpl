<!-- -*-HTML-*- -->
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
  $.getJSON("/coop/settings/allocate/method", function(fieldInfo){
    allocMethodFields.forEach(function(e){
      if(!fieldInfo.some(function(f){return f.ptrngFldLabel == e})){
        $("#" + e).remove()
        $("label[for='" + e + "']").remove()
      }
    })
  })
  var per = encodeURI($.url().param("period"))
  $('#addForm').ajaxForm({
       success: function(){
         window.location.href = sprintf("/control/members/patronage/period?period=%s", per)
       }
    })
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
<!-- remove seniority -->
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
