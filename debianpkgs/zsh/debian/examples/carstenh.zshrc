# ~/.zshrc file for zsh(1).
#
# This file is sourced only for interactive shells. It should contain
# commands to set up aliases, functions, options, key bindings, etc.
#
# Global Order: zshenv, zprofile, zshrc, zlogin
#
# To enable the below-mentioned features uncomment the according lines.


# ### Include user name, host name and current working directory in the prompt:
PS1='%(!..%n@)%m:%~%# '

# ### Select emacs like key bindings:
# bindkey -e

# ### Define some useful aliases:
# { ls --help | grep -- --color } >/dev/null 2>&1 && alias ls='ls --color=auto'
# alias l='ls -F'
# alias ll='ls -F -l'
# alias la='ls -F -A'

# ### Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
# HISTSIZE=1000
# SAVEHIST=1000
# HISTFILE=~/.zsh_history

# ### Teach less, e.g., reading compressed files and listing archive content:
# which lesspipe >/dev/null && eval "$(lesspipe)"

# ### Turn on completion with the default options:
# autoload -Uz compinit; compinit
# ### Enable completion menu:
# zstyle ':completion:*' menu select=2
# ### Activate colored completion:
# which dircolors >/dev/null && eval "$(dircolors -b)"
# zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# ### If we are in a Debian chroot display its name in the prompt:
# [[ -r /etc/debian_chroot ]] && : ${debian_chroot:="$(cat /etc/debian_chroot)"}
# PS1="${debian_chroot:+($debian_chroot)}%(!..%n@)%m:%~%# "

# ### Use vcs_info to include version control system information in the prompt:
# setopt prompt_subst
# autoload -Uz vcs_info; vcs_info 2>/dev/null && precmd() { vcs_info }
# PS1="${debian_chroot:+($debian_chroot)}%(!..%n@)%m:%~\${vcs_info_msg_0_}%# "
# ### Configure vcs_info to be colorful:
# zstyle ':vcs_info:*' actionformats ' %F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
# zstyle ':vcs_info:*' formats ' %F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
# zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'



# ### ########################################################################
# ### All features below this line are disabled, uncomment the according     #
# ### lines to enable them.                                                  #
# ### ########################################################################

# ### Content:
# ###  * Environment variables:
# ###     - LESS:          Set default options for less.
# ###     - GIT_PAGER:     Set pager used by git.
# ###     - GIT_EXEC_PATH: Tell git to look in ~/bin/git for git executables.
# ###     - TIMEFMT:       Set outputformat of shell builtin time.
# ###  * Zsh options:
# ###     - Not listed here.
# ###  * Precmd / preexec:
# ###     - Set xterm / rxvt title.
# ###     - Set screen's window title.
# ###     - Transparent directory replacement.
# ###  * Aliases:
# ###     - Not listed here.
# ###  * Functions:
# ###     - Start(), Restart(), Stop(), Reload(), Force-Reload():
# ###                   Start, restart, stop, reload or force-reload a System-V
# ###                   style init script.
# ###     - accessed(), changed(), modified():
# ###                   List files which have been changed / modified / accessed
# ###                   within the last n days, n defaults to 1.
# ###     - accessed.(), changed.(), modified.():
# ###                   Same as above, but list files whose name start with
# ###                   a dot.
# ###     - bk():       Make a backup of a file.
# ###     - cdt():      Create temporary directory and chdir into it.
# ###     - edalias():  Edit an alias via zle.
# ###     - edfunc():   Edit a function via zle.
# ###     - md():       Create given directory recursively and chdir into it.
# ###     - sll():      List symlinks in detail (more detailed 'readlink -f').
# ###     - unik():     Print unique lines, input does not need to be sorted.
# ###  * Misc:
# ###     - Chdir to ~ if zsh was started in a non-existent directory.
# ###     - Cat ~/dead.letter.
# ###     - Source ~/.zshrc.local if it exists and is readable.



# ### ########################################################################
# ### Environment variables:                                                 #
# ### ####################################################################{{{1

# ### Set default options for less:
# ###  -~  -- Display lines after end of file as blank lines.
# ###  -#  -- Specifies the default number of positions to scroll horizontally
# ###        in the RIGHTARROW and LEFTARROW commands.
# ###  -K  -- Causes less to exit immediately when ^C is typed.
# ###  -M  -- Causes less to prompt even more verbosely than more.
# ###  -R  -- Causes ANSI "color" escape sequences to be displayed.
# ###  -i  -- Causes searches to ignore case like vim's smartcase does.
# ###  -q  -- Use visual bell but not the terminal bell.
# ###  -w  -- Temporarily highlight the first "new" line after a forward movement.
# which less >/dev/null && export LESS='-~#20KMRiqw'

# ### Set pager used by git:
# ###  -E  -- Automatically exit the first time it reaches end-of-file.
# ###  -F  -- Automatically exit if the entire file fits on the first screen.
# ###  -X  -- Disable sending the termcap initialization and deinitialization
# ###         strings to the terminal.  Prevents clearing the screen.
# which less >/dev/null && export GIT_PAGER='less -EFX'

# ### Tell git to look in ~/bin/git for git executables:
# which git >/dev/null && export GIT_EXEC_PATH="`git --exec-path`:$HOME/bin/git"

# ### Set outputformat of shell builtin time:
# TIMEFMT="
#             Time spent in user mode:                    %U
#             Time spent in kernel mode:                  %S
#             Total time:                                 %E
#             CPU utilisation:                            %P
# "


# ### #####################################################################}}}
# ### Zsh options:                                                           #
# ### ####################################################################{{{1

# ### Changing directories:
# ### Try chdir if there is no matching executeable.
# setopt autocd
# ### Chdir to hashed directories without the need to prepend ~.
# setopt cdablevars
# ### Make cd push the old directory onto the directory stack.
# setopt autopushd
# ### Don't push multiple copies of the same directory onto the stack.
# setopt pushdignoredups
# ### Do not print the directory stack after pushd or popd.
# setopt pushdsilent
# ### Make pushd with no arguments act like `pushd $HOME'.
# setopt pushdtohome

# ### Completion:
# ### Use different widths whilst displaying completion menu to reduce size.
# setopt listpacked
# ### Try to complete when cursor is in the word.
# setopt complete_in_word
# ### Automatically list choices on an ambiguous completion.
# setopt autolist

# ### History:
# ### Don't display duplicates in while searching in history.
# setopt histfindnodups
# ### Don't put duplicate lines in history.
# setopt histignoredups
# ### Remove superfluous blanks from history.
# setopt histreduceblanks
# ### Reload line into editing buffer instead of executing it.
# setopt histverify
# ### Don't add lines prefixed by a space to history.
# setopt histignorespace

# ### Job Control:
# ### Don't nice backgrounded jobs.
# setopt nobgnice
# ### Don't send HUP signal to running jobs when the shell exists and don't
# ### complain about still running background jobs.
# setopt nohup nocheckjobs
# ### Print backgrounded jobs when they finish.
# setopt notify
# ### Disable flow-control with ^S and ^Q.
# setopt noflowcontrol

# ### Prompt:
# ### Print exitvalues != 0.
# setopt printexitvalue
# ### Send \r on new line.
# setopt promptcr

# ### Zle:
# ### Be quiet.
# setopt nobeep


# ### #####################################################################}}}
# ### Precmd / preexec:                                                      #
# ### ####################################################################{{{1

# ### Set xterm / rxvt title:
# preexec_xterm_title() {
#     [[ "$TERM" != "xterm" ]] && [[ "$TERM" == "${TERM#rxvt}" ]] && return
#     print -nR $'\033]0;'$1$'\a'
# }
# precmd_xterm_title() {
#     [[ "$TERM" != "xterm" ]] && [[ "$TERM" == "${TERM#rxvt}" ]] && return
#     print -nR $'\033]0;'Terminal$'\a'
# }
# preexec_functions=( ${preexec_functions} preexec_xterm_title )
# precmd_functions=(  ${precmd_functions}  precmd_xterm_title )

# ### Set screen's window title:
# preexec_screen_window_title() {
#     [[ "$TERM" == "${TERM#screen}" ]] && return
#     setopt localoptions extendedglob shwordsplit noksharrays
#     typeset -a cmd m_bracket m_brace m_paren m_percent
#     cmd=(${${1}[(wr)^(*=*|nice|sudo|time|env|fakeroot|trickle|-*),-1]})
#     [[ -n "$cmd[2]" ]] || { echo -ne "\ek$cmd[1]\e\\"; return; }
#     m_bracket=( vi vim emacs mcedit nano ee joe less more most )
#     m_percent=( make )
#     m_paren=( man perldoc )
#     m_brace=( )
#     local cmd1="$cmd[1]" m1 m2
#     if { [[ ${m_bracket[(i)$cmd1]} -le ${#m_bracket} ]] && m1='[' && m2=']' } \
#     || { [[ ${m_brace[(i)$cmd1]}   -le ${#m_brace}   ]] && m1='{' && m2='}' } \
#     || { [[ ${m_paren[(i)$cmd1]}   -le ${#m_paren}   ]] && m1='(' && m2=')' } \
#     || { [[ ${m_percent[(i)$cmd1]} -le ${#m_percent} ]] && m1='%' && m2='%' }
#     then
#         shift 1 cmd
#         cmd=(${${cmd}[(wr)^(*=*|-*|1|2|3|4|5|6|7|8),-1]})
#         cmd[1]="${${${${cmd[1]}##*/}%=}:-$cmd1}"
#     fi
#     echo -ne "\ek$m1$cmd[1]$m2\e\\"
# }
# precmd_screen_window_title() {
#     [[ "$TERM" == "${TERM#screen}" ]] && return
#     echo -ne "\ekzsh\e\\"
# }
# preexec_functions=( ${preexec_functions} preexec_screen_window_title )
# precmd_functions=(  ${precmd_functions}  precmd_screen_window_title )

# ### Transparent directory replacement:
# ### http://chris-lamb.co.uk/2009/11/19/transparent-directory-replacement-zsh/
# precmd_transparent_dir_replacement() {
#     [ . -ef "$PWD" ] && return 0
#     local OLDOLDPWD="$OLDPWD"
#     builtin cd -q -- "$PWD" >/dev/null 2>&1 || {
#         echo >&2 "W: $PWD does not exist anymore."
#         return 1
#     }
#     OLDPWD="$OLDOLDPWD"
# }
# precmd_functions=( ${precmd_functions} precmd_transparent_dir_replacement )


# ### #####################################################################}}}
# ### Aliases:                                                               #
# ### ####################################################################{{{1

# ### Ordinary aliases:
# { grep --help  | grep -- --color } >/dev/null 2>&1 && \
#     alias grep='grep --color=auto'
# { egrep --help | grep -- --color } >/dev/null 2>&1 && \
#     alias egrep='egrep --color=auto'
# alias cp='nocorrect cp'
# alias ln='nocorrect ln'
# alias mkdir='nocorrect mkdir'
# alias mv='nocorrect mv'
# alias rm='nocorrect rm'

# ### Suffix aliases:
# ### http://dev.codemac.net/config.git?p=config.git;a=blob;f=zsh/alias
# ### Automatically open images:
# if which feh >/dev/null; then
#     alias -s {jpg,JPG,jpeg,JPEG,png,PNG,gif,GIF}="feh -FZd"
# fi
# ### Automatically open movies:
# if which mplayer >/dev/null; then
#     alias -s {mpg,mpeg,avi,ogm,wmv,m4v,mp4,mov,3GP}="mplayer -idx"
# fi
# ### Automatically open web addresses (requires $BROWSER to be set):
# if [[ -n "$BROWSER" ]] && which "$BROWSER" >/dev/null; then
#     alias -s {html,htm,com,net,org,gov,edu,de}="$BROWSER"
# fi
# ### Automatically open text files (requires $EDITOR to be set):
# if [[ -n "$EDITOR" ]] && which "$EDITOR" >/dev/null; then
#     alias -s {txt,c,h}="$EDITOR"
# fi
# ### Automatically open other known files:
# which evince >/dev/null && alias -s pdf="evince"
# which evince >/dev/null && alias -s ps="evince"
# which java   >/dev/null && alias -s jar="java -jar"


# ### #####################################################################}}}
# ### Functions:                                                             #
# ### ####################################################################{{{1

# ### Start(), Restart(), Stop(), Reload(), Force-Reload():
# ###             Start, restart, stop, reload or force-reload a System-V
# ###             style init script.
# eval {Start,Restart,Stop,Reload,Force-Reload}'() { (
#     builtin cd -q / || { echo >&2 "E: Could not chdir to /"; return 1; }
#     env -i PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
#     "/etc/init.d/${1:?}" "${0:l}"
# ) };'
# compctl -g "/etc/init.d/*(:t)" Start Restart Stop Reload Force-Reload

# ### accessed(), changed(), modified():
# ###             List files which have been changed / modified / accessed
# ###             within the last n days, n defaults to 1.
# accessed()  { emulate -L zsh; print -l -- *(a-${1:-1}); }
# changed()   { emulate -L zsh; print -l -- *(c-${1:-1}); }
# modified()  { emulate -L zsh; print -l -- *(m-${1:-1}); }
# ### accessed.(), changed.(), modified.():
# ###             Same as above, but list files whose name starts with
# ###             a dot.
# accessed.() { emulate -L zsh; print -l -- .*(a-${1:-1}); }
# changed.()  { emulate -L zsh; print -l -- .*(c-${1:-1}); }
# modified.() { emulate -L zsh; print -l -- .*(m-${1:-1}); }

# ### bk():       Make a backup of a file.
# bk() {
#     cp -a "$1" "${1}_$(date --iso-8601=seconds)";
# }

# ### cdt():      Create temporary directory and chdir into it.
# cdt() {
#     [[ $# -eq 0 ]] || printf 'Usage: %s\n' "$0";
#     builtin cd "$(mktemp -t -d cdt.XXXXXXXXXX)";
# }

# ### edalias():  Edit an alias via zle.
# edalias() {
#    [[ -z "$1" ]] && { echo "Usage: edalias <alias_to_edit>" ; return 1 }
#    vared aliases'[$1]'
# }
# compdef _aliases edalias

# ### edfunc():   Edit a function via zle.
# edfunc() {
#    [[ -z "$1" ]] && { echo "Usage: edfun <function_to_edit>" ; return 1 }
#    zed -f "$1"
# }
# compdef _functions edfunc

# ### md():       Create given directory recursively and chdir into it.
# md() {
#     [[ $# -eq 1 ]] || printf 'Usage: %s <directory>\n' "$0";
#     mkdir -p "$1" && builtin cd "$1";
# }
# compdef md=mkdir

# ### sll():      List symlinks in detail (more detailed 'readlink -f').
# sll() {
#     [[ -z "$1" ]] && printf 'Usage: %s <file(s)>\n' "$0" && return 1
#     local file
#     for file in "$@"; do
#     (
#         while [[ -h "$file" ]]; do
#             ls -l "$file"
#             builtin cd -q "${file:h}"
#             file="$(readlink "${file:t}")"
#         done
#         ls -l "$file"
#     )
#     done
# }

# ### unik():     Print unique lines, input does not need to be sorted.
# unik() {
#     perl -ne 'print unless $seen{$_}++' "$@"
# }


# ### #####################################################################}}}
# ### Misc:                                                                  #
# ### ####################################################################{{{1

# ### Chdir to ~ if zsh was started in a non-existent directory:
# [[ "$PWD" == . ]] && builtin cd ~

# ### Cat ~/dead.letter:
# if [[ -r ~/dead.letter ]]; then
#     echo ~/dead.letter:
#     cat  ~/dead.letter
#     echo
# fi
# if [[ "$USERNAME" == root ]] && [[ -r /dead.letter ]]; then
#     echo /dead.letter:
#     cat  /dead.letter
#     echo
# fi

# ### Source ~/.zshrc.local if it exists and is readable:
# [[ -r ~/.zshrc.local ]] && . ~/.zshrc.local || true
