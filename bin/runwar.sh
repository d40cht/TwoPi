#!/bin/bash

# Install authbind to enable user bind to port 80
#
# sudo touch /etc/authbind/byport/80
# sudo chown alex:alex /etc/authbind/byport/80
# chmod 755 /etc/authbind/byport/80
#
# Then: ../../play-2.0.2/play stage
# generates: ./target/start
#
set -e
PORT=80 authbind --deep java -Xmx18000M -XX:+HeapDumpOnOutOfMemoryError -DapplyEvolutions.default=true -Djava.net.preferIPv4Stack=true -DLD_PRELOAD=/usr/lib/authbind/libauthbind.so.1 -jar -Dorg.scalatra.environment=production -Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.port=1098 -Dcom.sun.management.jmxremote.local.only=false jetty-runner-8.1.9.v20130131.jar --port 80 routesite_2.10-0.0.1.war 

