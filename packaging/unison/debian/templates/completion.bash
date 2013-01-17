_@UNISON_SELF_VAR@() {
  local cur profiles

  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}

  # Get option with unison -help | grep "^  -" | cut -f 3 -d " "

  if [[ "$cur" == -* ]]; then
      COMPREPLY=( $( compgen -W '-addprefsto -addversionno -auto -backup \
      -backupcurrent -backupcurrentnot -backupdir -backuplocation -backupnot \
      -backupprefix -backups -backupsuffix -batch -confirmbigdeletes \
      -confirmmerge -contactquietly -debug -doc -dumbtty -fastcheck -follow \
      -force -forcepartial -group -height -host -ignore -ignorecase \
      -ignorelocks -ignorenot -immutable -immutablenot -key -killserver -label \
      -log -logfile -maxbackups -maxthreads -merge -mountpoint -numericids \
      -owner -path -perms -prefer -preferpartial -pretendwin -repeat -retry \
      -root -rootalias -rsrc -rsync -selftest -servercmd -showarchive -silent \
      -socket -sortbysize -sortfirst -sortlast -sortnewfirst -sshargs -sshcmd \
      -terse -testserver -times -ui -version -xferbycopying' -- "$cur" ) )
    else
      if [ -d $HOME/.unison ]; then
        for i in $HOME/.unison/*.prf; do
          if test -e $i; then
            profiles="$profiles $(basename ${i%.prf})"
          fi
        done
      fi
      COMPREPLY=( $( compgen -W '$profiles' -- $cur ) );
    fi

    return 0
}

complete -o plusdirs -F _@UNISON_SELF_VAR@ @UNISON_SELF@
