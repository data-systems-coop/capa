<!-- -*-HTML-*- -->
<apply template="outerTemplate">
<script>$(document).ready(function(){ setupView() })</script>
<script>
function setupView(){
  loadCoop()
  loadAllocationSettings()  
  loadDisbursalSchedule()
}
</script>

<script>
function loadCoop(){
  $.getJSON("/coop", function(c){
    $("#name").append(c.name)
    $("#username").append(c.username)
    $("#fiscalCalendarType").append(
       sprintf("%s starting on month %s", 
       		c.fiscalCalendarType.periodType, c.fiscalCalendarType.start))
  })
}
</script>
<div class="row">
<div class="span7">
<h3><span id="name"/></h3>
<p> Username: <span id="username"/> </p>
<p> Fiscal Calendar Type: <span id="fiscalCalendarType"/> </p>
</div>
</div>

<script>
function loadAllocationSettings(){
  $.getJSON("/coop/settings/allocate/method", function(all){
    var fieldInfo = all[0]
    var method = all[1]
    var wghts = all[2]
    $("#allocMethod").append(method)
    allocMethodFields.forEach(function(e){
      if(!fieldInfo.some(function(f){return f.ptrngFldLabel == e})){
        $("#" + e).remove()
      }else{
        $("#" + e + "w").append(wghts[e + "w"])
      }
    })
  })
}
</script>
<div class="row">
<div class="span7">
<p> Allocation Method: <span id="allocMethod"/> </p>
<div id="work"><p> Hours Weight: <span id="workw"/> </p></div>
<div id="skillWeightedWork"><p> Wages Weight: 
  <span id="skillWeightedWorkw"/> </p></div>
<div id="seniority"><p> Seniority Weight: <span id="seniorityw"/> </p></div>
<div id="quality"><p> Quality Weight: 
  <span id="qualityw"/> </p></div>
<div id="revenueGenerated"><p> Revenue Generated Weight: 
  <span id="revenueGeneratedw"/> </p></div>
</div>
</div>


<script>
function formatDisbursal(d){
  return sprintf("<td>%s years %s months</td><td>%s %%</td>", 
           d[0].years, d[0].months , d[1]*100)
}
function loadDisbursalSchedule(){
  $.getJSON("/coop/settings/disburse/schedule/default", function(ds){
    $.each(ds, function(i, d) {
       $("#disbursalScheduleRows").append(
          sprintf("<tr>%s</tr>", formatDisbursal(d)))
    })
 })
}
</script>
<div class="row">
<div class="span7">
<p> Default Disbursal Schedule
<table class="table">
<thead><tr><th>After Allocation</th><th>Proportion</th> </thead>
<tbody id="disbursalScheduleRows"></tbody>
</table>
</div>
</div>

</apply>
