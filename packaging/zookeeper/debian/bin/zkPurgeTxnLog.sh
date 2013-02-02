#!/usr/bin/env sh
# This file has been written 2010 by thomas.koch@ymc.ch and is released under
# the following license:

# DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
# Version 2, December 2004
# 
# Copyright (C) 2004 Sam Hocevar
# 14 rue de Plaisance, 75014 Paris, France
# Everyone is permitted to copy and distribute verbatim or modified
# copies of this license document, and changing it is allowed as long
# as the name is changed.
# 
# DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
# TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
# 
# 0. You just DO WHAT THE FUCK YOU WANT TO.
# 
# Read more about this license at http://sam.zoy.org/wtfpl/

# You can also use it under any other license that complies to the Debian Free
# Software Guidelines

# This script should be called from a cron job to purge old log and snapshot
# files from zookeeper.

KEEPCOUNT=${KEEPCOUNT:-3}
ZKENV=${ZKENV:-"/etc/zookeeper/conf/environment"}
. $ZKENV
ZOOCFG=${ZOOCFG:-$ZOOCFGDIR/zoo.cfg}

if [ ! -r $ZOOCFG ]
then
  echo "$ZOOCFG is not readable"
  exit 1
fi

eval DATADIR=${DATADIR:-$(grep -e "^dataDir=" $ZOOCFG|sed s/.*dataDir.*=//)}
eval DATALOGDIR=${DATALOGDIR:-$(grep -e "^dataLogDir=" $ZOOCFG|sed s/.*dataLogDir.*=//)}

if [ ! -w $DATADIR ]
then
  echo "DATADIR $DATADIR is not writable"
  exit 1
fi

if [ "x$DATALOGDIR" = "x" ]
then
  DATALOGDIR=$DATADIR
elif [ ! -w $DATALOGDIR ]
then
  echo "DATALOGDIR $DATALOGDIR is not writable"
  exit 1
fi

java -cp $CLASSPATH $JVMFLAGS \
     org.apache.zookeeper.server.PurgeTxnLog $DATALOGDIR $DATADIR -c $KEEPCOUNT
