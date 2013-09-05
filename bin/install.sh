# not intended for automated execution yet

ON DB SERVER
-----INSTALL DB SERVER SOFTWARE------------
--install postgres + server
-----CREATE EMPTY DB INSTANCE-------
su - postgres
createuser capa_multi_tenant -P -S -d -R
createdb capa_multi_tenant -O capa_multi_tenant
-----CONFIGURE DB AUTHENTICATION-----
-- edit hba file to have a line allow client server: host ... md5
-- edit postgresql listen_addresses to allow client server


ON APPLICATION SERVER 
----INSTALL HASKELL COMPILER, PLATFORM-------
sudo apt-get install haskell-platform 
----INSTALL DB CLIENT SOFTWARE---------------
sudo apt-get postgresql-client postgresql-server-dev-9.1
----INSTALL SOURCE CONTROL CLIENT------------
sudo apt-get install git 
----CLONE REPO--------------------------
-- mkdir dev; cd dev
-- git clone ....
----UPDATE HASKELL LIBRARY DATABASE-------
-- switch to capa then: 
cd ~dev/capa; cabal update 
----INSTALL DEVELOPER EDITING TOOLS------
sudo apt-get install emacs haskell-mode
----BUILD CAPA---------------------------
cd ~dev/capa; cabal configure
(bin/stop.sh; cabal install)  --possibly multiple iterations of deleting Cabal db and adding library version constraints
mkdir ~/bin; cd ~/bin; ln -s ~/.cabal/bin/capa --create sym link
----CREATE PROD CONFIG FILE, ADD DB, LOGGING, SERVICE, UI RESOURCE CONFIGS----
-- fix config variables and config file name: db*. change name of config file in capa.hs. 
-- change liquibase credntials in db-update.sh and db-update-smoketest.sh
----REMOVE IRRELEVANT CONFIGS, SCRIPTS----------
rm bin/db-nuke.sh etc/dev.txt
-- INSTALL SCHEMA MANAGEMENT UTILITY -------------
wget "http://downloads.sourceforge.net/project/liquibase/Liquibase%20Core/liquibase-3.0.3-bin.tar.gz"; mv liqui* liq.tgz; tar -zxvf liq.tgz
mkdir ~/dev/capa/bin/ext/liquibase
cp -R liquibase liquibase.jar lib/ ~/dev/capa/bin/ext/liquibase/
cd ~/dev/capa/bin/ext/liquibase/lib
wget http://jdbc.postgresql.org/download/postgresql-9.2-1003.jdbc4.jar -- install postgres driver for liquibase
sudo apt-get install openjdk-7-jre -- install java! as root
-- TEST SCHEMA MANAGEMENT CONFIGURATION -----------
bin/db-update-smoketest.sh
-- PERFORM COMPLETE STOP, MIGRATE DB, START CYCLE
(bin/stop.sh; cabal install) && (rm nohup.out; bin/db-update.sh; bin/start.sh; sleep 0.5; tail nohup.out)
