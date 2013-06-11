<!DOCTYPE html>
<html>
<head>
<link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet" media="screen"/>
<script src="//code.jquery.com/jquery.js"></script>
<script src="//malsup.github.io/jquery.form.js"> </script> 
<script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"></script>
<script src="https://raw.github.com/alexei/sprintf.js/master/src/sprintf.min.js"></script>
</head>
<body>

<script>
function updateNavigation(selectItem){
  $(".navbar-inner ul li").removeClass("active")
  $(".navbar-inner ul li:contains('" + selectItem + "')").addClass("active")
}
</script>
<div class="navbar">
<div class="navbar-inner">
<a class="brand" href="#">CAPA</a>
<ul class="nav">
<li class="active"><a href="#">Home</a></li>
<li><a href="/control/member/patronage/add">Patronage</a></li>
<li><a href="/control/equity/members/allocate">Allocate</a></li>
</ul>
<ul class="nav pull-right">
<li><a href="#">Members</a></li>
<li><a href="#">Configure</a></li>
</ul>
</div>
</div>

<apply-content />

</body>
</html>
