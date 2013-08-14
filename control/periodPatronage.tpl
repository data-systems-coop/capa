<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  $.getJSON("/coop/settings/allocate/method", function(all){
    var fieldInfo = all[0]
    adjustPatronageHeaders(fieldInfo)
    var qs = $.url().param()
    fiscalPeriodPicker("#period", qs.period)
    $("#period").change(function(){
      $.getJSON("/members/patronage/" + encodeURI($("#period").val()), function(all){
        var unrecordedMembers = all[1]
        loadMemberPatronage(all[0], fieldInfo)
        setupUnrecordedPicker(all[1])
      })
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
function adjustPatronageHeaders(fieldInfo){
  allocMethodFields.forEach(function(e){
    if(!fieldInfo.some(function(f){return f.ptrngFldLabel == e})){
      $("#" + e).remove()
    }
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
    row += sprintf("<td>%s</td>", patronage[el.ptrngFldLabel])
  })
  $("#patronage").append(row + "</tr>")
}
</script>
<table class="table">
<thead><tr id="patronageHead">
  <th>Member</th><th id="work">Productive Hours</th>
  <th id="skillWeightedWork">Wages</th><th id="seniority">Seniority</th>
  <th id="quality">Quality</th><th id="revenueGenerated">Revenue Generated</th>
</tr></thead>
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
