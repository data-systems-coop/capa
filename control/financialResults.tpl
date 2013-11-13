<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function(){
  updateNavigation("Financial Results")
  loadAllFinancialResults()
})

function loadAllFinancialResults(){
  $("#results").empty()
  $.getJSON("/financial/results",function(data){
     $("#results").data("results", data)
     $.each(data, function(i,r){
       appendFinancialResults(r)
     })
   })
}
function removeFinancialResults(r){
  $.ajax(sprintf("/financial/results/%s",JSON.stringify(r.over)),
        {type: "DELETE",
         complete: loadAllFinancialResults})
}
function appendFinancialResults(res){
  var alloc = 
    (res.allocatedOn == null) ?
      sprintf(
        '<a href="/control/equity/members/allocationsDisbursals?over=%s"' + 
	' class="btn btn-primary">Allocate</a>', encodeURI(JSON.stringify(res.over))) :
      formatGregorianDay(res.allocatedOn)  //eventually link to allocation view
  var remove = 
    (res.allocatedOn == null) ?
        sprintf(
           "<a href='#' class='btn' onclick='removeFinancialResults(%s);'>Delete</a>",
           JSON.stringify(res)) : 
      ""
  var row = 
     sprintf('<tr><td>%s</td><td>$%s</td><td>%s</td><td>%s</td>', 
         formatFiscalPeriod(res.over), 
         res.surplus, 
         alloc,
         remove)
  $("#results").append(row)
}
</script>

<div class="row">
<div class="span5">
<table class="table">
<thead>
  <tr><th>Period</th><th>Surplus</th><th>Allocated</th><th></th>
</thead>
<tbody id="results">
</tbody>
</table>
</div>
</div>

<div class="row">
<div class="span2">
<a class="btn btn-primary"
   href="/control/financial/results/record">Record Result</a>
</div>
</div>

</apply>
