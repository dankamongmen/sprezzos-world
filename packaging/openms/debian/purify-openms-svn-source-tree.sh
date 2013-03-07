#!/bin/sh -e

workDir=$1

cd ${workDir}

# Remove all the .svn hidden dirs (source tree was cloned off a svn
# repos).
find -name ".svn" | xargs rm -rvf

# Remove all the exec bits from files that $(file) considers not
# binary.
for item in $(find -type f -executable)
do file ${item} | grep -i "elf "
    if [ "$?" != "0" ]
    then 
        echo "file ${item} not binary. Running chmod a-x on it."
        chmod a-x ${item}
    fi
done

