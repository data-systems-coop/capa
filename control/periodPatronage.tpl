<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  updateNavigation("Patronage")
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
  var row = sprintf("<tr><td>%s</td>", formatMember(member))
  fieldInfo.forEach(function(el){    
    row += sprintf("<td>%s</td>", patronage[el.ptrngFldLabel])
  })
  //should only be enabled if fiscal period is unallocated
  row += 
    sprintf("<td><a href='#' onclick='removePatronage(%s);'>x</a></td>",
            JSON.stringify(memberPatronage))
  $("#patronage").append(row + "</tr>")
}
function removePatronage(mp){
  var member = mp[0]
  var patronage = mp[1]
  $.ajax(sprintf('/member/%s/patronage/%s', 
                 member.memberId, 
                 encodeURI(JSON.stringify(patronage.performedOver))),
         {type:"DELETE", 
          complete: function(){ $("[name='period']").change() }})
}
</script>
<table class="table">
<thead><tr id="patronageHead">
  <th>Member</th><th id="work">Productive Hours</th>
  <th id="skillWeightedWork">Wages</th><th id="seniority">Seniority</th>
  <th id="quality">Quality</th><th id="revenueGenerated">Revenue Generated</th>
  <th></th>
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
    if(members.length == 0)
      $("#unrecordedPicker").hide()
    else
      $("#unrecordedPicker").show()
}
function appendMember(member){
  $("#memberId").append(
     sprintf("<option value='%s'>%s</option>", member.memberId, formatMember(member)))
}
</script>
<div class="row">
<div class="span3">

<div class="input-append" id="unrecordedPicker">
<select name="memberId" id="memberId"></select>
<button type="submit" class="btn">Enter Patronage</button>
</div>

</div>
</div>
</form>

</apply>
