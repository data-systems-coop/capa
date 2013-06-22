<apply template="outerTemplate">

<script>
</script>

<div class="row">
<div class="span3">

<form id="addMember" method="POST"> 
<label for="firstName">First Name</label>
<input type="text" class="input-small" name='firstName' id="firstName">
<label for="lastName">Last Name</label>
<input type="text" class="input-small" name='lastName' id="lastName">
<label for="acceptedOn">Accepted on</label>
<input type="text" class="input-mini" name='acceptedOn' id="acceptedOn">
<div class="form-actions">
<button type="submit" class="btn btn-primary">Add</button>
<button id="cancel" type="button" class="btn">Cancel</button>
</div>
</form>

</div>
</div>

</apply>