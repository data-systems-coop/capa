<!DOCTYPE html>
<html>
<head>
<script type="text/javascript">
document.write('<script type="text/javascript" src="' + ('https:'==document.location.protocol?'https://':'http://c.') + 'jslogger.com/jslogger.js"><\/script>');
</script>
<script type="text/javascript">
window.jslogger = new JSLogger({apiKey: "51fb259e1c07bf2772000018", track:true});
</script>
<link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css" rel="stylesheet" media="screen"/>
<script src="//code.jquery.com/jquery.js"></script>
<script src="//malsup.github.io/jquery.form.js"> </script> 
<script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"></script>
<script src="https://raw.github.com/alexei/sprintf.js/master/src/sprintf.min.js"></script>
<script src="https://raw.github.com/eternicode/bootstrap-datepicker/master/js/bootstrap-datepicker.js"></script> 
<script src="//cdnjs.cloudflare.com/ajax/libs/jquery-url-parser/2.3.1/purl.min.js"></script>
<script>

//library
function formatFiscalPeriod(per){
 return ( per.periodType == "Year" ) ? 
   sprintf("FY %s", per.start.year) :
   sprintf("FQ %s/%s", per.start.month, per.start.year)
}
function formatGregorianDay(day){
 return sprintf('%s/%s/%s', day[1], day[2], day[0])
}
// add buttons to go back, forward
function fiscalPeriodPicker(id, initial){
  $.getJSON("/fiscal/periods",function(periods){
    $.each(periods,function(i,period){
      addPeriod(period, id)
    })
    if(initial != undefined){
      $(id).val(initial)
    }
    $(id).change()
  })
}
function addPeriod(per, id){ 
  var opt = 
    sprintf("<option value='%s'>%s</option>", 
            JSON.stringify(per), 
            formatFiscalPeriod(per))
  $(id).append(opt)
}
function seniorityPicker(id){ //get rid of this altogether
  $.getJSON("/coop/settings/allocate/seniority/levels", function(levels){
    levels.forEach(function(lev){
      $(id).append(sprintf("<option value='%s'>%s</option>",lev[0].start, lev[1]))
    })
  })
}
function qualityPicker(id){
  var levels =  //should come from server
   [{name:"Average", value:1}, {name:"Good", value:2}, {name:"Great", value:3}]
  levels.forEach(function(lev){
   $(id).append(sprintf("<option value='%s'>%s</option>",lev.value, lev.name))
  })
}
var allocMethodFields = 
  ["work", "skillWeightedWork", "seniority", "quality", "revenueGenerated"]
function redirect(to){ window.location.href = to; }
</script>
</head>
<body>

<script>
function updateNavigation(selectItem){
  $(".navbar-inner ul li").removeClass("active")
  $(".navbar-inner ul li:contains('" + selectItem + "')").addClass("active")
}
</script>
<div class="navbar">  <!-- add login / user name inline form -->
<div class="navbar-inner">
<a class="brand" href="#">CAPA</a>
<ul class="nav">
<!-- finish menue -->
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
