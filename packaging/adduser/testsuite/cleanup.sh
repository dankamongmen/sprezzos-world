#!/bin/bash

####################################################################
# ATTENTION!! DANGER!!!
#
# This script will unconditionally zap all accounts, grups, mail
# and home directory data (including home directory contents) for 
# all accounts starting with $user_prefix.
####################################################################


FAILED=0

if [ "$(id -u)" != "0" ]; then
  echo "root needed"
  exit 1
fi

. ./commons.sh

if [ -z "$user_prefix" ]; then
  echo "no $user_prefix set"
  exit 1
fi

for acct in $(grep "^$user_prefix" /etc/passwd | awk '{print $1}' FS=":"); do
  echo $acct
  if [ -z "$acct" ]; then
    echo "empty \$acct in for loop. this should not happen"
    exit 1
  fi
  userdel $acct
  rm -rf /home/$acct
  rm -rf /var/spool/$acct
done

for grp in $(grep "^$user_prefix" /etc/group | awk '{print $1}' FS=":"); do
  echo $grp
  if [ -z "$grp" ]; then
    echo "empty \$grp in for loop. this should not happen"
    exit 1
  fi
  group $grp
done

rm -f $userid_file
