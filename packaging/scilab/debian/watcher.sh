#!/bin/sh

if [ "$#" -ne 4 ]
then
    echo "Watcher: Bad args: $#"
    exit 1
fi

CHECK_PID=$1
DIR="$2"
FILE="$3"
GREP_FOR="$4"

echo "Watcher: Check PID: $CHECK_PID"
echo "Watcher: Directory: $DIR"
echo "Watcher: File: $FILE"
echo "Watcher: Grep for: $GREP_FOR"

if [ -d "$DIR" ]; then
    if [ -f "$DIR/watcher-lock" ]; then
        echo "Watcher: not spawning as lock in place"
        exit 1
    else
        echo "Watcher: creating lock"
        touch $DIR/watcher-lock
    fi
fi

# If the PID we are told still exists (our caller is still running),
# the directory we are told (the working directory still exists) and
# the file we are told doesn't exist (the build stamp hasn't been created)
# keep going
while ps "$CHECK_PID" > /dev/null &&
      [ -d "$DIR" ] &&
      [ ! -f "$FILE" ]
do
    sleep 600
    echo "Watcher: Tick."
    ps ux | grep -- "$GREP_FOR" | grep -v grep
done

echo "Watcher: Terminating."
echo "Watcher: Removing lock"
[ -f $DIR/watcher-lock ] && rm $DIR/watcher-lock
