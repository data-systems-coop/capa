capa
====

Coop Accounting Patronage Allocation


    Version Scheme
    0.0.0.0
    | | | |
    | |-|-|-- breaks ?
      |-|-|-- breaks ? 
        |-|-- ? 
          |-- ? 

http://semver.org/
http://www.haskell.org/haskellwiki/Package_versioning_policy


Rebuild + Restart (I am losing stdout?): 
(bin/stop.sh; cabal install) && (rm nohup.out; bin/db-update.sh; bin/start.sh; sleep 0.5; tail nohup.out)


Download liquibase, extract to bin/ext/liquibase 
Download postgresql-java driver, extract to bin/ext/liquibase/lib