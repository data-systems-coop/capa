<apply template="outerTemplate">

<script>
</script>

<div class="row">
<div class="span8">
<div class="pagination">
<ul>
  <li><a href="#"><i class="icon-backward"/></a></li>
  <li><a href="#">Kanishka Azimi</a></li>
  <li class="active"><a href="#">Aaron Desrochers</a></li>
  <li><a href="#"><i class="icon-forward"/></a></li>
</ul>
</div>
</div>
</div>

<div class="row">
<div class="span5">
<label class="radio inline">
  <input type="radio" 
     name="accountTypeOptions" id="accountTypeRadio1" value="" checked>
      Rolling Patronage Account</label>
<label class="radio inline">
  <input type="radio" 
     name="accountTypeOptions" id="accountTypeRadio1" value="" checked>
      Committed Account</label>
</div>
</div>


<div class="row">
<div class="span7">

<table class="table">
<thead>
  <tr><th>Performed on</th><th>Type</th><th>Amount</th>
  <th>Result of</th><th>Balance</th></tr>
</thead>
<tbody id="result">
</tbody>
</table>

</div>
</div>

<div class="row">
<div class="span2">
<button type="submit" class="btn btn-primary">Add Action</button>
</div>
</div>

</apply>