#!/bin/sh

java -jar bin/ext/liquibase/liquibase.jar --driver=org.postgresql.Driver --classpath=bin/ext/liquibase/lib/postgresql-9.2-1003.jdbc4.jar --changeLogFile=db.changelog.sql --url="jdbc:postgresql://localhost/mydb" --username=kanishka --password=postgres validate
