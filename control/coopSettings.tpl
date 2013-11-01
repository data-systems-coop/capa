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
  // get alloc methods..
  $.getJSON("/allocate/methods", function(ms){
    $("[name='workw']").mask("900")
    $("[name='skillWeightedWorkw']").mask("900")
    $("[name='seniorityw']").mask("900")
    $("[name='reveneueGeneratedw']").mask("900")
    $("[name='qualityw']").mask("900")
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
        //if fieldInfo length == 1, then set weight = 100
      })
   })
   $("input[value='ProductiveHours']").prop('checked',true)
   $("[name='allocationMethod']").change()
  })
  loadSeniorityLevels([[{"start":0},1]])
  setupAddLevel()
  //scrap this, just use regular decimal number instead of %
  $('form').submit(function(){
    $("[name='workw']").val(intToPercent("[name='workw']"))
    $("[name='skillWeightedWorkw']").val(intToPercent("[name='skillWeightedWorkw']"))
    $("[name='seniorityw']").val(intToPercent("[name='seniorityw']"))
    $("[name='revenueGeneratedw']").val(intToPercent("[name='revenueGeneratedw']"))
    $("[name='qualityw']").val(intToPercent("[name='qualityw']"))
  })
  $('form').ajaxForm({
       success: function(){
	 window.location.href = sprintf("/control/coop/summary")
       }
    })
  //need to undo conversion on form failure
}
</script>
<form method="POST" action="/coop/settings/allocate"> 
<div class="row">
    <div class="span8" id="add">
          <div id="allocMethods"></div>
          
          <div class="work">
	  <label for="workw">Productive Hours Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='workw'>
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="skillWeightedWork">
	  <label for="skillWeightedWorkw">Wages Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='skillWeightedWorkw'>
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="seniority">
	  <label for="seniorityw">Seniority Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='seniorityw'>
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="revenueGenerated">
	  <label for="revenueGeneratedw">Revenue Generated Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='revenueGeneratedw'>
	    <span class="add-on">%</span>
	  </div>
          </div>

          <div class="quality">
	  <label for="qualityw">Quality Weight</label>
	  <div class="input-append">
	    <input type="text" class="input-mini" name='qualityw'>
	    <span class="add-on">%</span>
	  </div>
          </div>
    </div>
</div>


<script>
function formatLevel(l){
  return sprintf("<tr><td>%s</td><td>%s</td><td><a href='#' onclick='removeLevel(%s);'>x</a></td></tr>", l[0].start, l[1], JSON.stringify(l))
}
function loadSeniorityLevels(levels){
  $("#seniorityLevelRows").data("d", levels)
  saveToHidden()
  $("#seniorityLevelRows").empty()
  levels.forEach(function(l){
    $("#seniorityLevelRows").append(formatLevel(l))
  })
}
function saveToHidden(){
  $("#seniorityLevels").val(
     JSON.stringify($("#seniorityLevelRows").data("d")))
}
function removeLevel(l){
  var arr = $("#seniorityLevelRows").data("d")
  loadSeniorityLevels(arr.filter(function(v){return !(v[0].start == l[0].start)}))
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
	    <thead><tr><th>Start Year</th><th>Level</th><th></th></tr></thead>
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
function seniorityLevelPicker(id){
  var levels = [1,2,3,4,5,6,7]
  levels.forEach(function(l){
    $(id).append(sprintf("<option>%s</option>", l))
  })
}
function setupAddLevel(){
  yearCountPicker("#startYear")
  seniorityLevelPicker("#seniorityLevel")
  $("#addButton").click(function(){
    var startYear = parseInt($("#startYear").val())
    var seniorityLevel = parseInt($("#seniorityLevel").val())
    var arr = $("#seniorityLevelRows").data("d")
    loadSeniorityLevels(arr.concat([[{start:startYear}, seniorityLevel]]))
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
      <label for="seniorityLevel">Level</label>
      <select id="seniorityLevel"></select>
  </div>
  <div class="modal-footer">
    <button class="btn" data-dismiss="modal" aria-hidden="true">Cancel</button>
    <button type="button" id="addButton" class="btn btn-primary">Add</button>
  </div>
</div>



</apply>
