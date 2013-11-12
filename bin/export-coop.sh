COPY (
select *, 
       (select 
          string_agg( (afterAllocation).years || 'yrs' 
          || (afterAllocation).months || 'mos' || ' ' || proportion*100 || '%', ',') 
       from DisbursalSchedule where cpId = ?) as disbursalScheduleList 
       -- seniority levels here
from Cooperative 
inner join CoopSettings using (cpId) 
where cpId = ?) 
TO '/tmp/coopsettings.csv' WITH (FORMAT csv, HEADER);

COPY ( --fix to handle new form of actions
select * 
from Member m 
inner join MemberEquityAccount a using (cpId, mbrId) 
inner join MemberEquityAction c using (cpId,mbrId,acctId) 
where cpId = ? order by mbrId, acctId, performedOn) 
TO '/tmp/actions.csv' WITH (FORMAT csv, HEADER);

COPY (
select * 
from Member m 
inner join WorkPatronage p using (cpId, mbrId) 
where cpId = ? order by mbrId, (performedOver).prdStart) 
TO '/tmp/patronage.csv' WITH (FORMAT csv, HEADER);

COPY (
select * 
from FinancialResults 
where cpId = ? 
order by (rsltOver).prdStart) 
TO '/tmp/results.csv' WITH (FORMAT csv, HEADER);





