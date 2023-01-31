-- Drop table
drop table TABLE1 cascade constraints;
drop table TABLE2 cascade constraints;
drop table REF_TABLE cascade constraints;
drop table TABLE3 cascade constraints;
-- Create table
create table TABLE1
(
  id            NUMBER not null,
  value_char    VARCHAR2(100),
  unique_name_1 VARCHAR2(200) not null,
  dt            DATE,
  dt_ts         TIMESTAMP(6),
  dt_tstz       TIMESTAMP(6) WITH TIME ZONE,
  dt_tsltz      TIMESTAMP(6) WITH LOCAL TIME ZONE
);
-- Add comments to the columns 
comment on column TABLE1.id  is 'Some comment on table1';
comment on column TABLE1.dt  is 'This is date column';
-- Create/Recreate primary, unique and foreign key constraints 
alter table TABLE1  add constraint TABLE1_PK primary key (ID)  using index;
alter table TABLE1  add constraint TABLE1_UK1 unique (UNIQUE_NAME_1) using index;

-- Create table
create table TABLE2
(
  id    NUMBER not null,
  value VARCHAR2(200)
);
-- Create/Recreate primary, unique and foreign key constraints 
alter table TABLE2  add constraint TABLE2_PK primary key (ID)  using index ;

-- Create table
create table REF_TABLE
(
  table1_id NUMBER not null,
  table2_id NUMBER not null,
  ref_value VARCHAR2(100)
);
-- Create/Recreate primary, unique and foreign key constraints 
alter table REF_TABLE  add constraint REF_TABLE_PK primary key (TABLE1_ID, TABLE2_ID)  using index ;
alter table REF_TABLE  add constraint REF_TABLE_TABLE1_FK foreign key (TABLE1_ID)  references TABLE1 (ID);
alter table REF_TABLE  add constraint REF_TABLE_TABLE2_FK foreign key (TABLE2_ID)  references TABLE2 (ID);

-- Create table
create table TABLE3
(
  id             NUMBER not null,
  table1_id      NUMBER not null,
  table2_id      NUMBER not null,
  ref_table_data VARCHAR2(100)
);
-- Add comments to the columns 
comment on column TABLE3.ref_table_data  is 'Some comment';
-- Create/Recreate primary, unique and foreign key constraints 
alter table TABLE3  add constraint TABLE3_PK primary key (ID)  using index ;
alter table TABLE3  add constraint REF_TABLE_FK foreign key (TABLE1_ID, TABLE2_ID) references REF_TABLE (TABLE1_ID, TABLE2_ID);

