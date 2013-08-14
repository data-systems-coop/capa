<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function(){
  updateNavigation("Financial Results")
  loadAllFinancialResults()
})

function loadAllFinancialResults(){
  $.getJSON("/financial/results",function(data){
     $("#results").data("results", data)
     $.each(data, function(i,r){
       appendFinancialResults(r)
     })
   })
}
function appendFinancialResults(res){
  var alloc = 
    (res.allocatedOn == null) ?
      sprintf(
        '<a href="/control/equity/members/allocationsDisbursals?over=%s"' + 
	' class="btn btn-primary">Allocate</a>', encodeURI(JSON.stringify(res.over))) :
      formatGregorianDay(res.allocatedOn)  //eventually link to allocation view
  var row = 
     sprintf('<tr><td>%s</td><td>$%s</td><td>%s</td>', 
         formatFiscalPeriod(res.over), 
         res.surplus, 
         alloc)
  $("#results").append(row)
}
</script>

<div class="row">
<div class="span5">
<table class="table">
<thead>
  <tr><th>Period</th><th>Surplus</th><th>Allocated</th>
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
