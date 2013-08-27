<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  setupForm()
})
</script>


<script>
function setupForm(){
  // get alloc methods..
  $.getJSON("/allocate/methods", function(ms){
    $.each(ms, function(i, m){
      $("#allocMethods").append(
       sprintf("<label class='radio inline'>" + 
               "<input type='radio' name='allocationMethod' value='%s'>%s</label>", 
               m, m))
     })
   $("input[name='allocationMethod']").change(function(){
    $.getJSON("/allocate/method/" + $("input[name='allocationMethod']:checked").val(), 
      function(fieldInfo){
        allocMethodFields.forEach(function(e){
         if(!fieldInfo.some(function(f){return f.ptrngFldLabel == e})){
           $("." + e).hide()
          }else{
           $("." + e).show()
          }
        })
      })
   })
   $("input[value='ProductiveHours']").prop('checked',true)
   $("input[name='allocationMethod']").change()
  })
  loadSeniorityLevels([{"start":0,"end":2},{"start":3,"end":5}])
  setupAddLevel()
  $('form').ajaxForm({
       success: function(){
	 window.location.href = sprintf("/control/coop/summary")
       }
    })
}
</script>
<form method="POST" action="/coop/settings/allocate"> 
<div class="row">
    <div class="span8" id="add">
          <div id="allocMethods"></div>
          
          <div class="work">
	  <label for="workw">Productive Hours Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='workw' id="workw">
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="skillWeightedWork">
	  <label for="skillWeightedWorkw">Wages Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='skillWeightedWorkw' id="skillWeightedWorkw">
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="seniority">
	  <label for="seniorityw">Seniotity Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='seniorityw' id="seniorityw">
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="revenueGenerated">
	  <label for="workw">Revenue Generated Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='revenueGeneratedw' id="revenueGeneratedw">
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="quality">
	  <label for="workw">Quality Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='qualityw' id="qualityw">
	    <span class="add-on">%</span>
	  </div>
          </div>
    </div>
</div>
	  <script>
	  function formatLevel(level){
	    return sprintf("%s - %s", level.start, level.end)
	  }
	  function loadSeniorityLevels(levels){
	    $("#seniorityLevelRows").data("d", levels)
	    saveToHidden()
	    $("#seniorityLevelRows").empty()
	    levels.forEach(function(l){
	      $("#seniorityLevelRows").append(sprintf("<tr><td>%s</td><td>X</td></tr>", formatLevel(l)))
	    })
          }
	  function saveToHidden(){
	    $("#seniorityLevels").val(JSON.stringify($("#seniorityLevelRows").data("d")))
	  }
	  </script>  

<div class="seniority">
<div class="row">
  <div class="span3">
	  <label>Seniority Levels</label>
  </div>
</div>
<div class="row">
    <div class="span2">	  
	  <input type="hidden" name="seniorityLevels" id="seniorityLevels">
	  <table class="table">
	    <thead><tr><th>Years</th><th></th></tr></thead>
 	    <tbody id="seniorityLevelRows"></tbody>
	  </table>
	  <button type="button" class="btn btn-small" data-toggle="modal" data-target="#addModal">Add Level</button>
    </div>
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
function setupAddLevel(){
  yearCountPicker("#startYear")
  yearCountPicker("#endYear")
  $("#addButton").click(function(){
    var startYear = $("#startYear").val()
    var endYear = $("#endYear").val()
    var arr = $("#seniorityLevelRows").data("d")
    loadSeniorityLevels(arr.concat({start:startYear, end:endYear}))
    $("#addModal").modal('hide')
  })
}
</script>	
<div id="addModal" class="modal hide fade" tabindex="-1" 
	     role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-header">
    <button type="button" class="close" data-dismiss="modal" aria-hidden="true">x</button>
    <h3 id="myModalLabel">Add Level</h3>
  </div>
  <div class="modal-body">
      <label for="startYear">Start Year</label>
      <select id="startYear"></select>
      <label for="endYear">End Year</label>
      <select id="endYear"></select>
  </div>
  <div class="modal-footer">
    <button class="btn" data-dismiss="modal" aria-hidden="true">Cancel</button>
    <button type="button" id="addButton" class="btn btn-primary">Add</button>
  </div>
</div>



</apply>
