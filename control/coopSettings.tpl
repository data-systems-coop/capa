<apply template="outerTemplate">

<script>
$(document).ready(function() {
  setupForm()
})
</script>


<script>
function setupForm(){
  loadSeniorityLevels([{"start":0,"end":2},{"start":3,"end":5}])
  $('#updateForm').ajaxForm({
       success: function(){
	 var per = uriEncode($.url().param("period"))
	 window.location.href = sprintf("/control/members/patronage/period?period=%s", per)
       }
    })
}
</script>
<form id="updateForm" method="POST"> 
<div class="row">
    <div class="span5" id="add">
	  <label class="radio inline">
	    <input type="radio" name='allocationMethod' id="allocationMethod1" value="ProductiveHours" checked>Productive Hours
	  </label>
	  <label class="radio inline">
	    <input type="radio" name='allocationMethod' id="allocationMethod1" value="Wages">Wages
	  </label>

	  <label for="workw">Productive Hours Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='workw' id="workw">
	    <span class="add-on">%</span>
	  </div>

	  <label for="skillWeightedWorkw">Wages Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='skillWeightedw' id="skillWeightedw">
	    <span class="add-on">%</span>
	  </div>

	  <label for="seniorityWeight">Seniotity Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='seniorityw' id="seniorityw">
	    <span class="add-on">%</span>
	  </div>

	  <label for="workw">Revenue Generated Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='seniorityw' id="seniorityw">
	    <span class="add-on">%</span>
	  </div>

	  <label for="workw">Quality Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='qualityw' id="qualityw">
	    <span class="add-on">%</span>
	  </div>
    </div>
</div>
	  <script>
	  function formatLevel(level){
	    return sprintf("%s - %s", level.start, level.end)
	  }
	  function loadSeniorityLevels(levels){
	    //save in hidden
 	    //display in table
	    levels.forEach(function(l){
	      $("#seniorityLevelRows").append(sprintf("<tr><td>%s</td><td>X</td></tr>", formatLevel(l)))
	    })
          }
	  function showAddDialog(){
	    //create modal
	    //add form + selects
	    //register add and cancel handlers
	  }
	  </script>  
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
	<div id="addModal" class="modal hide fade" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
	<div class="modal-header">
	<button type="button" class="close" data-dismiss="modal" aria-hidden="true">×</button>
	<h3 id="myModalLabel">Modal header</h3>
	</div>
	<div class="modal-body">
	<p>One fine body…</p>
	</div>
	<div class="modal-footer">
	<button class="btn" data-dismiss="modal" aria-hidden="true">Close</button>
	<button class="btn btn-primary">Save changes</button>
	</div>
    </div>
    </div>
</div>	  
<div class="row">
    <div class="span5">
	  <div class="form-actions">
	    <button type="submit" class="btn btn-primary">Save</button>
	    <a class="btn" href="/control/coopSummary">Cancel</a> 
	  </div>
    </div>
</div>
</form>

</apply>
