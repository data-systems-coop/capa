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

