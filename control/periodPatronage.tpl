<apply template="outerTemplate">

<script>
$(document).ready(function() {
  //get method -> name. use name to get field list
  var calcMethodFieldInfo = 
    [{name:"work", label:"Productive Hours"},
     {name:"skillWeightedWork", label:"Wages"},
     {name:"seniority", label:"Seniority"},
     {name:"quality", label:"Quality"},
     {name:"revenueGenerated", label:"Revenue Generated"}]
  createPatronageHeaders(calcMethodFieldInfo)
  fiscalPeriodPicker("#period")
  $("#period").change(function(){
    $.getJSON("/members/patronage/" + encodeURI($("#period").val()), function(all){
      var unrecordedMembers = all[1]
      loadMemberPatronage(all[0], calcMethodFieldInfo)
      setupUnrecordedPicker(all[1])
    })
  })
})
</script>

<form id="goToRecord" method="GET" action="/control/member/patronage/record">
<div class="row">
<div class="span3"><select name="period" id="period"></select></div>
</div>


<div class="row">
<div class="span8">
<script>
function createPatronageHeaders(fieldInfo){
  fieldInfo.forEach(function(el){
    $("#patronageHead").append(sprintf("<th>%s</th>",el.label))
  })
}
function loadMemberPatronage(memberPatronage, fieldInfo){
  $("#patronage").empty()
  $.each(memberPatronage, function(i,m){
    appendPatronage(m, fieldInfo)
  })
}
function appendPatronage(memberPatronage, fieldInfo){
  var member = memberPatronage[0]
  var patronage = memberPatronage[1]
  var row = sprintf("<tr><td>%s</td>", member.firstName)
  fieldInfo.forEach(function(el){    
    row += sprintf("<td>%s</td>", patronage[el.name])
  })
  $("#patronage").append(row + "</tr>")
}
</script>
<table class="table">
<thead><tr id="patronageHead"><th>Member</th></tr></thead>
<tbody id="patronage">
</tbody>
</table>
</div>
</div>

<script>
function setupUnrecordedPicker(members){
    $("#memberId").empty()
    $.each(members, function(i,member){
      appendMember(member)
    })   
}
function appendMember(member){
  $("#memberId").append(
     sprintf("<option value='%s'>%s</option>", member.memberId, member.firstName))
}
</script>
<div class="row">
<div class="span3">

<div class="input-append">
<select name="memberId" id="memberId"></select>
<button type="submit" class="btn">Enter Patronage</button>
</div>

</div>
</div>
</form>

</apply>
