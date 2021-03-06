<!DOCTYPE html>
<html>
<head>
<script type="text/javascript">
document.write('<script type="text/javascript" src="' + ('https:'==document.location.protocol?'https://':'http://c.') + 'jslogger.com/jslogger.js"><\/script>');
</script>
<script type="text/javascript">
window.jslogger = new JSLogger({apiKey: "5202804a4d1c96595a000025", track:true});
</script>
<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-45079819-1', 'camp.coop');
  ga('send', 'pageview');
</script>
<link href="/js/bootstrap/bootstrap.min.css" rel="stylesheet" media="screen"/>
<link href="/js/bootstrap-datepicker/bootstrap-datepicker.min.css" rel="stylesheet"/>
<script src="/js/jquery/jquery.js"></script>
<script src="/js/jquery.form/jquery.form.js"></script> 
<script src="/js/bootstrap/bootstrap.min.js"></script>
<script src="/js/bootstrap-datepicker/bootstrap-datepicker.min.js"></script>
<script src="/js/sprintf/sprintf.min.js"></script>
<script src="/js/purl/purl.min.js"></script>
<script src="/js/jquery.mask/jquery.mask.min.js"></script>
<script>

//library
function datePickerDflt(id){
  $(id).datepicker({format:'mm/dd/yyyy',autoclose:true})
  $(id).mask("00/00/0000")
}
function formatFiscalPeriod(per){
 return ( per.periodType == "Year" ) ? sprintf("FY %s", per.start.year) : 
        ( per.periodType == "Quarter" ) ? 
           sprintf("FQ %s/%s", per.start.month, per.start.year) : 
         sprintf("FM %s/%s", per.start.month, per.start.year)
}
function formatGregorianDay(day){
 return sprintf('%s/%s/%s', day[1], day[2], day[0])
}
function fiscalPeriodPicker(id, initial){// add buttons to go back, forward
  $.getJSON("/fiscal/periods",function(periods){
    $.each(periods,function(i,period){
      addPeriod(period, id)
    })
    if(initial != undefined){
      $(id).val(initial)
    }else{
      var current = new Date()
      $(id).val(JSON.stringify(closestPeriod(current, periods)))
    }
    $(id).change()
  })
}
function closestPeriod(asOf, periods){ //require periods.length > 0
  var diffs = 
    $.map(
      periods, 
      function(p){
        return [[Math.abs(asOf.getTime() - gregorianMonthToDate(p.start).getTime()), p]]
      })
  var closest = diffs[0]
  $.each(diffs, function(_,d){
    if(d[0] < closest[0])
      closest = d
    else
      closest = closest
  })
  return closest[1]
}
function gregorianMonthToDate(g){
  return new Date(g.year,g.month-1)
}

function formatMember(mbr){
 return sprintf("%s %s", mbr.firstName, mbr.lastName)
}
function addPeriod(per, id){ 
  var opt = 
    sprintf("<option value='%s'>%s</option>", 
            JSON.stringify(per), 
            formatFiscalPeriod(per))
  $(id).append(opt)
}
function qualityPicker(id){
  var levels =  //should come from server
   [{name:"Average", value:1}, {name:"Good", value:2}, {name:"Great", value:3}]
  levels.forEach(function(lev){
   $(id).append(sprintf("<option value='%s'>%s</option>",lev.value, lev.name))
  })
}
var actionTypes = 
    [{label:"Buy In", value:"BuyIn"}, 
     //{label:"Allocate Patronage Rebate", value:"AllocatePatronageRebate"},
     //{label:"Distribute Installment", value:"DistributeInstallment"},
     {label:"Earn Interest", value:"EarnInterest"},
     {label:"Distribute on Departure", value:"DistributeOnDeparture"},
     {label:"Distribute on Dissolution", value:"DistributeOnDissolution"},
     //{label:"Distribute Milestone", value:"DistributeMilestone"},
     {label:"Allocate Delayed Non Qualified", value:"AllocateDelayedNonQualified"}]
function actionTypePicker(id){
  actionTypes.forEach(function(typ){
    $(id).append(sprintf("<option value='%s'>%s</option>",typ.value, typ.label))
  })
}
function formatAccountType(typeId){
  //object map of value -> label pairs
  //lookup(value)
}

function amountFormat(action){
  return (["BuyIn", "AllocatePatronageRebate", 
           "EarnInterest", 
           "AllocateDelayedNonQualified"].indexOf(action.actionType) != -1 ? 
          1 : -1 ) * action.amount
}
function monthPicker(id){
  [[1,"January"],[2,"February"],[3,"March"],[4,"April"],
    [5,"May"],[6,"June"],[7,"July"],[8,"August"],[9,"September"],
    [10,"October"],[11,"November"],[12,"December"]].forEach(function(m){
    $(id).append(sprintf("<option value='%s'>%s</option>", m[0], m[1]))
  }) 
}
var allocMethodFields = 
  ["work", "skillWeightedWork", "seniority", "quality", "revenueGenerated"]
function intToPercent(id){
  //if no value, set to 0
  return parseInt($(id).val()) / 100
}

function redirect(to){ window.location.href = to; }
</script>
<style type="text/css">
.container {
  margin: 0 auto;
  max-width: 850px;
}
</style>
</head>
<body>

<script>
function updateNavigation(selectItem){
  $("ul li").removeClass("active")
  $("ul li:contains('" + selectItem + "')").addClass("active")
}
function disableNavigation(){
  $("ul li").removeClass("active")
  $("ul li").addClass("disabled")
}
function logout(){
  $.post("/logout", function(){redirect("/")})
}
</script>

<div class="container">
<ul class="nav nav-tabs">
<li><a href="/control/coop/summary">Home</a></li>
<li><a href="/control/members/accounts">Member Accounts</a></li>
<li><a href="/control/members/patronage/period">Patronage</a></li>
<li><a href="/control/financial/results">Financial Results</a></li>
<li><a href="/control/coop/settings/show">Settings</a></li>
<li><a href="#" onclick='logout();'>Logout</a></li>
</ul>


<apply-content />
</div>

</body>
</html>
