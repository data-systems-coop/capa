capa
====

Coop Accounting Patronage Allocation

    Version Scheme
    x.y.z-prereleasetag
    | | | |
    |-|-|-|-- break public api (x > 0)
      |-|-|-- backwards compatible, new functionality 
        |-|-- backwards compatible, bug fix 
          |-- to indicate almost reaching the version, but not tested enough to know if really compatible

http://semver.org/ . http://www.haskell.org/haskellwiki/Package_versioning_policy

Versioned APIs: 
Service API - parameter types/arity, result types, paths/names
UI - form element names, urls, data views, exports. menu organization and layout are not considered part of guaranteed interface
for now, product version reflects UI versions only

Rebuild + Restart (I am losing stdout?): 

    (bin/stop.sh; cabal install) && (rm nohup.out; bin/db-update.sh; bin/start.sh; sleep 0.5; tail nohup.out)

