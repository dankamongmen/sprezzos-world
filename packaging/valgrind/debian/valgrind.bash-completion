#-*- mode: shell-script;-*-

# Debian GNU/Linux valgrind(1) completion

have valgrind &&
_valgrind()
{
    local cur prev opts yesno tools choosen_tool
    
# list of options with yes|no answer
    yesno="--trace-children\= --child-silent-after-fork\= --track-fds\= --time-stamp\= --log-fd\=  --log-socket\="

    opts="--tool\= "
    opts+="-h --help --help-debug --version -q --quiet -v --verbose --log-file\= "


    COMPREPLY=()

# we check if a tool has been defined 
    for (( i=0; i < ${#COMP_WORDS[@]}-1; i++ )); do
      if [[ ${COMP_WORDS[i]} == "--tool" ]]; then
          choosen_tool=${COMP_WORDS[i+2]}
      fi
    done
# if so, we add its options for the completion
    case $choosen_tool in
      'callgrind')
        opts+="--callgrind-out-file\= --dump-every-bb\= --dump-before\= --zero-before\= --dump-after\= --toggle-collect\= --separate-recs\= --separate-callers\= --fn-skip\= --fn-group< --separate-recs< --separate-callers< "
        yesno+="--dump-instr\= --dump-line\= --compress-strings\= --compress-pos\= --combine-dumps\= --instr-atstart\= --collect-atstart\= --collect-jumps\= --separate-threads\= --skip-plt\= --simulate-cache\= --simulate-hwpref\= "
        ;; 
      'cachegrind')
        opts+="--I1\= --D1\= --L2\= --cachegrind-out-file\= "
        yesno+="--cache-sim\= --branch-sim\= "
        ;;
# as memcheck is the tool by default, we show its options if no tool is specified
      'memcheck')
          yesno+="--undef-value-errors\= --track-origins\= --show-reachable\= --workaround-gcc296-bugs\= --partial-loads-ok\= "
          opts+="--leak-check\= --leak-resolution\= --freelist-vol\= --malloc-fill\= --free-fill\= "
       ;;
      *)
          yesno+="--undef-value-errors\= --track-origins\= --show-reachable\= --workaround-gcc296-bugs\= --partial-loads-ok\= "
          opts+="--leak-check\= --leak-resolution\= --freelist-vol\= --malloc-fill\= --free-fill\= "
      ;;
    esac
    
    opts+="$yesno "

    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD - 1]}"
# COMPREPLY considers '=' as a word. For $prev we prefer the word before the actual "="
    if [[ "$prev" == "=" ]]
    then
        prev="${COMP_WORDS[COMP_CWORD - 2]}"
    elif [[ "$cur" == "=" ]]
    then
        cur=""
    fi


# autocompletion for the tool names, e.g. memcheck, callgrind
    if [[ "$prev" == "--tool" ]]
    then
# the list is automatically built 
# it is composed of all the executable file in
# /usr/lib/valgrind/x86-linux
# TODO: correct the path during the installation, according to the platform
            tools="memcheck cachegrind callgrind massif helgrind lackey none drd exp-bbv exp-ptrcheck" 
            COMPREPLY=( $(compgen -W "$tools" -- $cur ) )
            COMPREPLY+=" "
            return 0
    fi

    if [[ "${yesno}" == *$prev* ]] 
    then
        COMPREPLY=( $(compgen -W "yes no" -- $cur ) )
        COMPREPLY+=" "
        return 0
    fi

    case "$prev" in
        --log-file)
            _filedir
            return 0
            ;;
        *)
            ;;
    esac
 
# Look for options without an argument.
    if [[ "$cur" == -* ]]
    then
        COMPREPLY=( $(compgen -W "$opts" -- $cur) )
        return 0
    else
        _filedir
        return 0
    fi
}
complete -o nospace -F _valgrind valgrind

