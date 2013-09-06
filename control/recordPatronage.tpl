<!-- -*-HTML-*- -->
<apply template="outerTemplate">

<script>
$(document).ready(function() {
  updateNavigation("Patronage")
  loadParameters()
  setupForm()
})
</script>

<div class="row">
<div class="span3" id="add">
<script>
function loadParameters(){
  var qs = $.url().param()
  $.getJSON("/member/" + qs.memberId, function(mem){
    $("#memberName").append(formatMember(mem))
    $("#performedOver").append(formatFiscalPeriod(JSON.parse(qs.period)))
    $('form').attr("action",sprintf("/member/%s/patronage/%s", qs.memberId, qs.period))
  })
}
function setupForm(){
  qualityPicker("[name='quality']")  
  $("[name='work']").mask("9900", {reverse:true})
  $("[name='skillWeightedWork']").mask("999000", {reverse:true})
  $("[name='revenueGenerated']").mask("9999000", {reverse:true})
  $.getJSON("/coop/settings/allocate/method", function(all){
    var fieldInfo = all[0]
    allocMethodFields.forEach(function(e){
      if(!fieldInfo.some(function(f){return f.ptrngFldLabel == e})){
        $("[name='" + e + "']").remove()
        $("label[for='" + e + "']").remove()
        $("#" + e).remove()
      }
    })
  })
  var per = encodeURI($.url().param("period"))
  $('form').ajaxForm({
       success: function(){
         window.location.href = 
           sprintf("/control/members/patronage/period?period=%s", per)
       }
    })
  $("a:contains('Cancel')").attr(
                   "href",
                   sprintf("/control/members/patronage/period?period=%s",per))
}
</script>
<form method="POST"> 

<p id="memberName"></p>
<p id="performedOver"></p>
<input type="text" name='work' placeholder="Productive Hours" class="input-medium">
<div class="input-prepend" id="skillWeightedWork">
  <span class="add-on">$</span>
  <input type="text" name='skillWeightedWork' placeholder="Wages" class="input-small">
</div>
<label for="quality">Quality</label>
<select class="input-small" name='quality'></select>
<div class="input-prepend" id="revenueGenerated">
  <span class="add-on">$</span>
  <input type="text" name='revenueGenerated' placeholder="Revenue Generated" class="input--medium"> 
</div>
<div class="form-actions">
<button type="submit" class="btn btn-primary">Save</button>
<a class="btn">Cancel</a> 
</div>
</form>

</div>
</div>

</apply>
