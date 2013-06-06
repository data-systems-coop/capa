<html>
<head>
<link href="css/bootstrap.min.css" rel="stylesheet" media="screen">
<script src="http://code.jquery.com/jquery.js"></script>
<script src="http://malsup.github.com/jquery.form.js"></script> 
<script src="js/bootstrap.min.js"></script>
<script>
</script>
</head>
<body>

<div class="row">
<div class="span8">

<table class="table table-hover">
<thead>
  <tr><th>Name</th><th></th></tr>
</thead>
<tbody>
  <tr><td>m1</td><td><i class="icon-ok"></i></td></tr>
  <tr class="info"><td>m2</td><td>Add</td></tr>
</tbody>
</table>

</div>

<div class="span3">

<form id="myForm" action="http://localhost:8000/member/m1/patronage/1"> 
<label>Work<input class="input-mini" name='work'/></label>
<label>Skill-Weighted Work<input class="input-mini" name='skillWeightedWork'/></label>
<label>Seniority<input class="input-mini" name='seniority'/></label>
<label>Quality<input class="input-mini" name='quality'/></label>
<label>Revenue Generated<input class="input-mini" name='revenueGenerated'/></label>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<button type="button" class="btn">Cancel</button>
</div>
</form>

</div>

</div>
</body>
</html>
