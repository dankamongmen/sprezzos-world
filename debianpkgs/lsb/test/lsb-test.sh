#!/bin/sh -e

echo "Importing $1/init-functions"
. $1/init-functions

log_warning_msg "Only a warning"
log_success_msg "This should succeed"
log_failure_msg "This fails miserably"

echo "OK!"
