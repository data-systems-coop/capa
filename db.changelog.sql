--liquibase formatted sql

--changeset kazimi:import  context:prod
create type FiscalPeriod AS (prdStart date, prdType varchar(20));

create table Cooperative(
  cpId int not null,
  PRIMARY KEY(cpId)
);

create table CoopSettings(
  cpId integer not null references Cooperative(cpId),
  PRIMARY KEY(cpId)
);

create table DisbursalSchedule(
  cpId integer not null references Cooperative(cpId),
  afterAllocation integer not null,
  PRIMARY KEY(cpId, afterAllocation)
);

create table FinancialResults(
  cpId integer not null references Cooperative(cpId),
  rsltOver FiscalPeriod not null,
  surplus integer not null,
  allocatedOn date,
  PRIMARY KEY(cpId, rsltOver)  
);

create table Member(
  cpId integer not null references Cooperative(cpId),
  mbrId integer not null,
  firstName varchar(50) not null,
  PRIMARY KEY(mbrId, cpId)
);

create table WorkPatronage(
  cpId integer not null,
  mbrId integer not null,
  performedOver FiscalPeriod not null,
  work integer not null,
  skillWeightedWork integer not null,
  quality integer not null,
  revenueGenerated integer not null,
  PRIMARY KEY(cpId,mbrId,performedOver),
  FOREIGN KEY(cpId,mbrId) REFERENCES Member(cpId,mbrId)
);

create table MemberEquityAccount(
  cpId integer not null,
  mbrId integer not null,
  acctId integer not null,
  acctType varchar(30) not null,
  PRIMARY KEY(cpId,mbrId,acctId),
  FOREIGN KEY(cpId,mbrId) references Member(cpId,mbrId)
);

create table MemberEquityAction(
  cpId integer not null,
  mbrId integer not null,
  acctId integer not null,
  acnType varchar(30) not null,
  amount integer not null,
  performedOn date not null,
  resultOf FiscalPeriod null,
  PRIMARY KEY(cpId,mbrId,acctId,acnType,amount,performedOn,resultOf),
  FOREIGN KEY(cpId,resultOf) references FinancialResults(cpId,rsltOver)
);


--changeset kazimi:import2  context:test
insert into Cooperative values 
  (1)
, (2)
, (3);

insert into FinancialResults values
  (1, ('2012-01-01','Year'), 200, '2011-01-02')
, (2, ('2013-01-01','Year'), 300, null);

insert into Member values 
  (1, 1, 'John')
, (1, 2, 'Kanishka')
, (1, 3, 'Aaron');

insert into WorkPatronage values
  (1, 1, ('2012-01-01','Year'), 5, 4, 3, 2)
, (1, 2, ('2012-01-01','Year'), 10, 20, 30, 40)
, (1, 3, ('2012-01-01','Year'), 100, 200, 300, 400);

insert into MemberEquityAccount values
  (1, 1, 1, 'Committed')
, (1, 1, 2, 'RollingPatronage')
, (1, 2, 1, 'Committed')
, (1, 2, 2, 'RollingPatronage')
, (1, 3, 1, 'Committed')
, (1, 3, 2, 'RollingPatronage')


--changeset kazimi:settings context:prod
alter table CoopSettings add column allocationMethod varchar(40) not null;
alter table CoopSettings add column work numeric(3,2) not null;
alter table CoopSettings add column skillWeightedWork numeric(3,2) not null;
alter table CoopSettings add column seniority numeric(3,2) not null;
alter table CoopSettings add column quality numeric(3,2) not null;
alter table CoopSettings add column revenueGenerated numeric(3,2) not null;
  

--changeset kazimi:settingsDisburse context:prod
alter table DisbursalSchedule drop constraint disbursalschedule_pkey;
alter table DisbursalSchedule drop column afterAllocation;

create type GregorianDuration as (years integer, months integer);
alter table DisbursalSchedule add column afterAllocation GregorianDuration not null;
alter table DisbursalSchedule add constraint disbursalschedule_pkey
      PRIMARY KEY(cpId, afterAllocation);
alter table DisbursalSchedule add column proportion numeric(3,2) not null;


--changeset kazimi:settings2 context:test
insert into CoopSettings values 
 (1, 'SimpleMix', 0.70, 0.30, 0, 0, 0);

--changeset kazimi:settingsDisburse2 context:test
insert into DisbursalSchedule values
  (1,(1,0),0.6), (1,(1,6),0.4);


--changeset kazimi:seniorityLevels context:prod
create table SeniorityMappings(
  cpId integer not null references Cooperative(cpId),
  startYear integer not null,
  snrtyMpngLevel integer not null,
  PRIMARY KEY(cpId, startYear)
);


--changeset kazimi:coop context:prod
create type FiscalCalendarType AS (start int, prdType varchar(20));

alter table Cooperative 
    add column cpName varchar(130)
  , add column username varchar(150)
  , add column usageStart date
  , add column usgaeEnd date
  , add column fiscalCalendarType FiscalCalendarType;

update Cooperative 
set cpName = 'coop' || cpId
  , username = 'https://www.google.com/profiles/102678013619698873278'
  , usageStart = '2012-01-01'
  , fiscalCalendarType = (1,'Year');

alter table Cooperative 
    alter column cpName set not null
  , alter column userName set not null
  , alter column usageStart set not null
  , alter column fiscalCalendarType set not null;


--changeset kazimi:coop-fix context:prod
alter table Cooperative rename column usgaeEnd to usageEnd;


--changeset kazimi:finres-fk-hack context:prod
alter table MemberEquityAction drop constraint memberequityaction_cpid_fkey;


--changeset kazimi:member-expand context:prod
alter table Member
      add column lastName varchar(100)
    , add column acceptedOn date;

update Member
set lastName = 'unknown'
  , acceptedOn = '2011-01-01';

alter table Member
       alter column lastName set not null
     , alter column acceptedOn set not null
     , add constraint member_uq UNIQUE (cpId, firstName, lastName, acceptedOn);


--changeset kazimi:rename-committed context:prod
update MemberEquityAccount 
   set acctType = 
       case acctType when 'Committed' then 'BuyInAcct' 
       	    	     when 'RollingPatronage' then 'RollingPatronageAcct' 
      end 
where acctType in ('Committed', 'RollingPatronage');


--changeset kazimi:alloc-group context:prod
create table Allocation( 
       cpId integer not null, 
       resultOf FiscalPeriod not null, 
       alcPerformedOn date not null,
       PRIMARY KEY(cpId,resultOf),
       FOREIGN KEY(cpId,resultOf) references FinancialResults(cpId,rsltOver)
);
create table MemberAllocateAction( 
       cpId integer not null, 
       mbrId integer not null, 
       acctId integer not null, 
       allocatedRatio numeric(3,2) not null, 
       resultOf FiscalPeriod not null, 
       PRIMARY KEY(cpId,mbrId,acctId),
       FOREIGN KEY(cpId,mbrId) references Member(cpId,mbrId),
       FOREIGN KEY(cpId,resultOf) references Allocation(cpId,resultOf)
);
create table Disbursal( 
       cpId integer not null, 
       resultOf FiscalPeriod not null, 
       dsbPerformedOn date not null, 
       dsbProportion numeric(3,2) not null,
       PRIMARY KEY(cpId,resultOf,dsbPerformedOn),
       FOREIGN KEY(cpId,resultOf) references Allocation(cpId,resultOf)
); -- manually migrate financial results, actions

