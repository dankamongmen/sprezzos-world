: ${GNUPGHOME=$HOME/.gnupg}

GPGAGENT=/usr/bin/gpg-agent
PID_FILE="$GNUPGHOME/gpg-agent-info-$(hostname)"

if grep -qs '^[[:space:]]*use-agent' "$GNUPGHOME/gpg.conf" "$GNUPGHOME/options" &&
   test -x $GPGAGENT &&
   { test -z "$GPG_AGENT_INFO" || ! $GPGAGENT 2>/dev/null; }; then

   if [ -r "$PID_FILE" ]; then
       . "$PID_FILE"
   fi

   # Invoking gpg-agent with no arguments exits successfully if the agent
   # is already running as pointed by $GPG_AGENT_INFO
   if ! $GPGAGENT 2>/dev/null; then
       STARTUP="$GPGAGENT --daemon --sh --write-env-file=$PID_FILE $STARTUP"
   fi
fi
