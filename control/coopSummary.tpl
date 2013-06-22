<apply template="outerTemplate">

<script>
function setupForm(){
  $('.datepicker').datepicker({format: 'mm-dd-yyyy'})
}
$(document).ready(setupForm)
</script>

<div class="row">
<div class="span5">
<h3>Data Systems [3/1984 - Present]</h3>
</div>
</div>

<div class="row">
<div class="span8">

<table class="table">
<thead>
  <tr><th>Member</th><th>Equity</th><th>Accepted on</th><th>Left on</th></tr>
</thead>
<tbody id="result">
  <tr><td>Aaron Desrochers</td><td>$5,210</td><td>4/2009</td>
    <td><input type="text" class="datepicker input-small">
        <button type="submit" class="btn btn-small">Leave</button>   </td></tr>
  <tr><td>Kanishka Azimi</td><td>$300</td><td>3/2009</td>
    <td>4/1/2009</td></tr>
</tbody>
</table>

</div>
</div>


<div class="row">
<div class="span3">
<button type="submit" class="btn">New Member</button>
</div>
<div class="span2">
<label class="checkbox"> 
  <input type="checkbox">Show Deactivated</label>
</div>
</div>

</apply>