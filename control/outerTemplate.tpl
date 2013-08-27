<!DOCTYPE html>
<html>
<head>
<script type="text/javascript">
document.write('<script type="text/javascript" src="' + ('https:'==document.location.protocol?'https://':'http://c.') + 'jslogger.com/jslogger.js"><\/script>');
</script>
<script type="text/javascript">
window.jslogger = new JSLogger({apiKey: "5202804a4d1c96595a000025", track:true});
</script>

<link href="//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/css/bootstrap.min.css" rel="stylesheet" media="screen"/>
<link href="//cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.1.3/css/bootstrap-datepicker.min.css" rel="stylesheet"/>
<script src="//code.jquery.com/jquery.js"></script>
<script src="//malsup.github.io/jquery.form.js"> </script> 
<script src="//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/2.3.2/css/bootstrap.min.css"></script>
<script src="//cdnjs.cloudflare.com/ajax/libs/bootstrap-datepicker/1.1.3/js/bootstrap-datepicker.min.js"></script>
<script src="https://raw.github.com/alexei/sprintf.js/master/src/sprintf.min.js"></script>
<script src="//cdnjs.cloudflare.com/ajax/libs/jquery-url-parser/2.3.1/purl.min.js"></script>
<script>

//library
function datePickerDflt(id){
  $(id).datepicker({format:'mm/dd/yyyy',autoclose:true})
}
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
var actionTypes = 
    [{label:"Buy In", value:"BuyIn"}, 
     {label:"Allocate Patronage Rebate", value:"AllocatePatronageRebate"},
     {label:"Distribute Installment", value:"DistributeInstallment"},
     {label:"Earn Interest", value:"EarnInterest"},
     {label:"Distribute on Departure", value:"DistributeOnDeparture"},
     {label:"Distribute on Dissolution", value:"DistributeOnDissolution"},
     {label:"Distribute Milestone", value:"DistributeMilestone"},
     {label:"Allocate Delayed Non Qualified", value:"AllocateDelayedNonQualified"}]
function actionTypePicker(id){
  actionTypes.forEach(function(typ){
    $(id).append(sprintf("<option value='%s'>%s</option>",typ.value, typ.label))
  })
}
function monthPicker(id){
  [1,2,3,4,5,6,7,8,9,10,11,12].forEach(function(m){
    $(id).append(sprintf("<option value='%s'>%s</option>", m, m))
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
  $("ul li").removeClass("active")
  $("ul li:contains('" + selectItem + "')").addClass("active")
}
function disableNavigation(){
  $("ul li").removeClass("active")
  $("ul li").addClass("disabled")
}
</script>

<ul class="nav nav-tabs">
<li><a href="/control/coop/summary">Home</a></li>
<li><a href="/control/financial/results">Financial Results</a></li>
<li><a href="/control/members/patronage/period">Patronage</a></li>
<li><a href="/control/members/accounts">Member Accounts</a></li>
<li><a href="/control/coop/settings/show">Settings</a></li>
<li><a href="/control/logout">Logout</a></li>
</ul>


<apply-content />

</body>
</html>
