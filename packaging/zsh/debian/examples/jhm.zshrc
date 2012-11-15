# Ray's .zshrc
# Note that completions for dpkg-deb, mutt, mount, and dupload are
# included as part of the new completion system.  See docs for details.

# Zsh settings

# TODO
# - completions for dpkg-deb
# - completions for lintian
# - completions for dupload
# - completions for nm
# - completions for objdump
# - completions for mount
# - completion for man -l

bindkey -e	# Emacs-style commandline editing

case "$TERM" in
	linux)	# Linux console
		bindkey '\e[1~' beginning-of-line	# Home 
		bindkey '\e[4~' end-of-line		# End  
		bindkey '\e[3~' delete-char		# Del
		bindkey '\e[2~' overwrite-mode		# Insert  
#		bindkey '\e[A' up-line-or-history	# cursor up
#		bindkey '\e[B' down-line-or-history	# cursor down
#		bindkey '\e[C' forward-char		# cursor right
#		bindkey '\e[D' backward-char		# cursor left
		;;
	screen) # The textmode window manager
		# In Linux console
		bindkey '\e[1~' beginning-of-line	# Home
		bindkey '\e[4~' end-of-line		# End  
		bindkey '\e[3~' delete-char		# Del
		bindkey '\e[2~' overwrite-mode		# Insert  
		bindkey '\e[7~' beginning-of-line	# home
		bindkey '\e[8~' end-of-line		# end
		# In rxvt
		bindkey '\eOc' forward-word		# ctrl cursor right
		bindkey '\eOd' backward-word		# ctrl cursor left
		bindkey '\e[3~' backward-delete-char	# This should not be necessary!
#		bindkey '\e[A' up-line-or-history	# cursor up
#		bindkey '\e[B' down-line-or-history	# cursor down
#		bindkey '\e[C' forward-char		# cursor right
#		bindkey '\e[D' backward-char		# cursor left
		;;
	rxvt)
		bindkey '\e[7~' beginning-of-line	# home
		bindkey '\e[8~' end-of-line		# end
		bindkey '\eOc' forward-word		# ctrl cursor right
		bindkey '\eOd' backward-word		# ctrl cursor left
		bindkey '\e[3~' backward-delete-char	# This should not be necessary!
		bindkey '\e[2~' overwrite-mode		# Insert
		;;
	xterm*)
		bindkey '\e[H' beginning-of-line	# Home
		bindkey '\e[F'  end-of-line		# End
		bindkey '\e[3~' delete-char		# Del
		bindkey '\e[2~' overwrite-mode		# Insert
		;;
	sun)
		bindkey '\e[214z' beginning-of-line       # Home
		bindkey '\e[220z' end-of-line             # End
		bindkey '^J'      delete-char             # Del
		bindkey '^H'      backward-delete-char    # Backspace
		bindkey '\e[247z' overwrite-mode          # Insert
		;;
esac

#if test "X$TERM" = "Xscreen" || test "X$TERM" = "Xlinux"; then
#  bindkey '\e[A' up-line-or-history	# cursor up
#  bindkey '\e[B' down-line-or-history	# cursor down
#  bindkey '\e[C' forward-char		# cursor right
#  bindkey '\e[D' backward-char		# cursor left
#fi
#
#if test "X$TERM" = "Xscreen" || test "X$TERM" = "Xrxvt"; then
#  bindkey '\e[7~' beginning-of-line	# home
#  bindkey '\e[8~' end-of-line		# end
#  bindkey '\eOc' forward-word		# ctrl cursor right
#  bindkey '\eOd' backward-word		# ctrl cursor left
#  bindkey '\e[3~' backward-delete-char	# This should not be necessary!
#fi

export DEBEMAIL=jhm@cistron.nl
export IRCNICK=JHM
export IRCSERVER=irc.nl.openprojects.org
eval `lesspipe`

alias glimpse='glimpse -z -i -w'

# All functions used for defining completion rules start with `c_'.
setopt extendedglob	# required for some of our completion magic
setopt list_types	# Put a file type indicator after file names when 
			# completing.
setopt correct		# Attempt spelling correction on command names only.
setopt print_exit_value
setopt list_ambiguous
setopt bash_auto_list	# Make <tab><tab> do the right thing.
setopt list_packed	# Compacter display of completion list.
setopt interactive_comments
setopt nohup		# So backgrounded jobs don't get terminated when
			# the shell is.

# A sensible prompt:
# hostname username(underlined) time pwd(bold; max 30 chars.) > or #
if test "X$TERM" = "Xscreen"; then
	# include a hint that screen(1) can use to set window titles.
	PROMPT='%m %U%n%u %T %B%30<..<%~%b %{[0000mk\%}%(!.#.>) '
else
	# We don't include the hint outside screen, as it causes problems
	# with xterm
	PROMPT='%m %U%n%u %T %B%30<..<%~%b %(!.#.>) '
fi

c_groups=( $(cut -d: -f1 /etc/group) )

############################################################################
# Shell builtins

compctl -k '( )' pushln
compctl -z -P '%' bg
compctl -j -P '%' fg jobs disown
compctl -j -P '%' + -s '`ps -x | tail +2 | cut -c1-5`' wait

compctl -A shift
compctl -caF type whence which
compctl -c unhash
compctl -F functions unfunction
compctl -x 'w[1,-d] p[2]' -n - 'w[1,-d] p[3]' -g '*(-/)' - \
        'p[1]' -c - 'p[2]' -g '*(-x)' -- hash
compctl -a unalias
compctl -v getln getopts read unset vared
compctl -v -S '=' -q declare export integer local readonly typeset
compctl -e disable
compctl -d enable

eval compctl -k "'("`limit | cut -d\  -f1`")'" limit unlimit

compctl -l '' -x 'p[1]' -f -- . source
compctl -s '`unsetopt`' setopt
# Redirection below makes zsh silent when completing unsetopt xtrace
compctl -s '`setopt 2> /dev/null`' unsetopt
compctl -s '${^fpath}/*(N:t)' autoload

compctl -b bindkey
compctl -c -x 'C[-1,-*k]' -A - 'C[-1,-*K]' -F -- compctl
compctl -x 'C[-1,-*e]' -c - 'C[-1,-[ARWI]##]' -f -- fc
compctl -x 'p[1]' - 'p[2,-1]' -l '' -- sched
compctl -x 'C[-1,[+-]o]' -o - 'c[-1,-A]' -A -- set

# Anything after nohup is a command by itself with its own completion
compctl -l '' nohup exec nice eval sudo fakeroot
compctl -x 'p[1]' -c - 'p[2,-1]' -k signals -- trap
compctl -l '' -x 'p[1]' -B -- builtin

# kill takes signal names as the first argument after -, but job names after %
# or PIDs as a last resort
compctl -j -P '%' + -s '`ps -x | tail +2 | cut -c1-5`' + \
        -x 's[-] p[1]' -k "($signals[1,-3])" -- kill


############################################################################
# Basic commands and file manipulation commands

compctl -k '( )' pwd
compctl -g '*(/)' cd
compctl -g '^*.(c|cc|C|cxx|cpp|h|in|l|y|tex)' rm
compctl -g '*(/)' rmdir
compctl -g '*.Z' + -g '*(/)' znew
compctl -s '$(groups)' + -k groups newgrp
compctl -f -x 'p[1], p[2] C[-1,-*]' -k groups -- chgrp
compctl -f -x 'p[1] n[-1,.], p[2] C[-1,-*] n[-1,.]' -k groups \
        - 'p[1], p[2] C[-1,-*]' -u -S '.' -- chown

# GNU ls, dir: complete files, options (both - and -- kind), and option params.
compctl -f \
  -x s'[--format]' -P '=' -k '(long verbose commas horizontal across vertical si
ngle-column)' \
  - s'[--sort]' -P '=' -k '(none time size extension)' \
  - s'[--time]' -P '=' -k '(atime ctime access use status)' \
  - s'[--width=][--tabsize=][--ignore=][-w][-T][-I],c[-1,-w][-1,-T][-1,-I]' \
    -k '( )' \
  - s'[--]' -S '' -k '(all\  escape\  directory\  inode\  kilobytes\  numeric-uid-gid\  no-group\  hide-control-chars\  reverse\  size\  width= tabsize= almost-all\  ignore-backups\  classify\  file-type\  full-time\  ignore= dereference\ literal\  quote-name\  dired\  no-color\  7bit\  8bit\  recursive\  sort= format= time= no-group\  help\  version\ )' \
  - s'[-]' -k '(a b c d f g i k l m n o p q r s t u x A B C F G L N Q R S U X 1
w T I)' \
  -- ls dir


# Debian tar: viewing/extracting gzipped/compressed/bzip2ed/normal tar archives.
compctl -f -x \
	'C[-1,*[xt]*f*z*] p[2]' -g '*.tar.gz *.tar.Z *.tgz' + -g '*(/)' - \
	'C[-1,*[xt]*f*I*] p[2]' -g '*.tar.bz *.tar.bz2 *.tbz' + -g '*(/)' - \
	'C[-1,*[xt]*f*] p[2]' -g '*.tar' -- \
	+ -g '*(/)' tar

## tar: complete tar files (only .tar or .tar.* format) after -f, disable
## completion for certain options, let user choose directory with -C,
## complete GNU tar long options beginning with --.  The match-taropts
## function prompts for GNU tar options, ensures one of the seven
## mandatory options is given in the first argument to tar, and enforces
## spaces after options that take an argument (this is required by GNU
## tar and also makes filename completion possible for the -f option).
## Note that the -[0-7][lmh] options are not completed, but they're
## hardly ever used.
#compctl -f \
#  -x 'C[-1,-*f],p[2] C[-1,*f],c[-1,--file]' -g '*.tar(|.*)' + -g '*(-/)' \
#  - 'C[-1,-*[bLN]],p[2] C[-1,*[bLN]],c[-1,--block-size][-1,tape-length][-1,--after-date][-1,--newer]' -k '( )' \
#  - 'C[-1,-*C],p[2] C[-1,*C],c[-1,directory]' -g '*(-/)' \
#  - 'C[-1,-*[FgKTV]],p[2] C[-1,*[FgKTV]],c[-1,--info-script][-1,--new-volume-script][-1,--starting-file][-1,--files-from][-1,--label][-1,--exclude]' -f \
#  - 's[--]' -k '(catenate concatenate create diff compare delete append list update extract get atime-preserve block-size read-full-blocks directory checkpoint file force-local info-script new-volume-script incremental dereference ignore-zeros ignore-failed-read keep-old-files starting-file one-file-system tape-length modification-time multi-volume after-date newer old-archive portability to-stdout same-permissions preserve-permissions absolute-paths preserve record-number remove-files same-order preserve-order same-owner sparse files-from null totals verbose label version interactive confirmation verify exclude exclude-from compress uncompress gzip ungzip use-compress-program block-compress)' \
#  - 's[-],p[1]' -S '' -K 'match-taropts' \
#  -- tar

# GNU find.
# Note that 'r[-exec,;]' must come first.
# We hardwire the filesystem types rather than use /proc/filesystems, in order
# not to exclude filesystems that are supported through modules that are
# currently not loaded.
compctl -x 'r[-exec,;][-ok,;]' -l '' - \
's[-]' -s 'daystart {max,min,}depth follow noleaf version xdev mount \
        {a,c,}newer {a,c,m}{min,time} empty false {fs,x,}type {u,g}id inum \
	links {i,}{l,}name {no,}{user,group} {i,}path perm {i,}regex size \
	true used exec {f,}print{f,0,} ok prune {f,}ls or not and' - \
'p[1]' -g '. .. *(-/)' - \
'C[-1,-((a|c|)newer|fprint(|0|f))]' -f - \
'c[-1,-fstype]' -k '(ext2 fat iso9660 minix nfs vfat)' - \
'c[-1,-type]' -k '(b c d p f l s)' -X '[b]lock dev, [c]har dev, [d]ir, named [p]ipe, regular [f]ile, [s]ocket' - \
'c[-1,-size]' -k '(1234c 567k)' -X '512-byte [b]locks, [c] bytes, [k]ilobytes, 2-byte [w]ords' - \
'c[-1,-group]' -k c_groups - \
'c[-1,-user]' -u -- find


############################################################################
# Commands that are usually used on non-generated files.

compctl -g '*.gz *.Z *.bz2' \
	+ -g '*.web *.docbook' \
	+ -g '*.c *.cc *.C *.cxx *.cpp *.h *.tex *.txt *.html' \
	+ -g '^*.(o|a|so|aux|dvi|log|swp|fig|bbl|blg|bst|idx|ind|out|toc)' \
	+ -g '.*' \
	+ -g '*(/)' vi vim gvim less grep zgrep

############################################################################
# Mail
# TODO: mutt

############################################################################
# Document processing

compctl -g '*.tex' + -g '*(/)' tex
compctl -g '*.tex *.ltx' + -g '*(/)' {latex,pdflatex} 
compctl -g '*.texi' + -g '*(/)' texi2{dvi,pdf,html}

function c_bibtex {
	reply=(`ls *.aux | sed -e 's/\.aux//'`);
}
compctl -K c_bibtex bibtex


############################################################################
# Viewers, editors etc.

compctl -g '*.gz *.Z' + -g '*(/)' zcat gunzip gzcat zless
compctl -g '*.bz *.bz2' + -g '*(/)' bzcat bz2cat bunzip2
compctl -g '*.ps *.ps.gz *.pdf *.pdf.gz *.eps *.eps.gz' + -g '*(/)' gv
compctl -g '*.(e|)ps' + -g '*(-/)' gs ghostview ps2ascii ps2text psnup ps2pdf
compctl -g '*.pdf' + -g '*(/)' acroread xpdf
compctl -g '*.dvi *.dvi.gz' + -g '*(/)' xdvi
compctl -g '*.dvi' + -g '*(/)' dvi2fax dvidvi dvilj dvilj4 dvips dvitomp \
	dvicopydvihp dvilj2p dvilj4l dvired dvitype
compctl -g '*.html' + -g '*.htm' + -g '*(/)' w3m lynx mozilla netscape
compctl -g '*.zip' + -g '*(/)' unzip
compctl -g '*.fig' + -g '*(/)' xfig
compctl -g '*.rtf' + -g '*(/)' Ted
compctl -g '*.web' + -g '*(/)' tangle weave ftangle fweave ctangle cweave ctanglex cweavex

# man: complete commands, otherwise complete by search of $MANPATH.
# This is placed as an all-encompassing pattern at the end because making it
# the default before the -x doesn't work.  (It becomes
# '-c + (-K 'match-man' -x ...), not (-c + -K 'match-man') -x ...).
# We also complete paths for -M (override manpath), commands for -P (pager) and
# disable for -S (search sections).  After an explicit number (which it helps
# to complete for you), these completion rules assume a thorough search is
# needed and no longer use the '-c' hashed commands, relying entirely on
# what's really in the manpath.
# Also support -l <file> and -T<device>
c_man_var() {
   man_pages=( /usr/share/man/man*/*(N:t:r) /usr/man/man*/*(N:t:r) )
   compctl -k man_pages man
   reply=( $man_pages )
}
compctl -x 'S[1][2][3][4][5][6][7][8][9]' -k '(1 2 3 4 5 6 7 8 9)' \
  - 'R[[1-9nlo]|[1-9](|[a-z]),^*]' -K 'match-man' \
  - 's[-M],c[-1,-M]' -g '*(-/)' \
  - 's[-P],c[-1,-P]' -c \
  - 's[-S],s[-1,-S]' -k '( )' \
  - 's[-l],c[-1,-l]' -g '*.[0-9] *.man' \
  - 's[-T]' -k '(X100 X75 ascii latin1 ps X100-12 X75-12 dvi lj4)' \
  - 's[-]' -k '(a d f h k t M P l)' \
  - 'p[1,-1]' -c + -K 'c_man_var' \
  -- man

function c_enscript_langs {
# languages and file formats for which enscript supports pretty-printing
	reply=(`enscript --help-pretty-print | grep '^Name:' | sed -e 's/Name: //'`)
}
compctl -g '*' + -g '*(/)' -x s'[-E][--pretty-print]' -P '=' -K c_enscript_langs \
  - s'[--]' -S '' -k '(columns= pages= file-align= header= no-header\  truncate-lines\  line-numbers\  setpagedevice= escapes\  pretty-print\  pretty-print= font= header-font= fancy-header\  no-job-header\  highlight-bars= indent= filter= borders\  page-prefeed\  no-page-prefeed\  lineprinter\  lines-per-page= mail\  media= copies= newline= missing-characters\  output= printer= quiet\  silent\  landscape\  portrait\  baselineskip= statusdict= title=  tabsize= underlay\  nup=  verbose\  version\  language= encoding= no-formfeed\  pass-through\  ps-level= rotate-even-pages\  toc\  word-wrap\ )' \
	-- enscript

############################################################################
# Networking

function c_lftp_bookmarks {
# extract the names of bookmarks from the lftp bookmarks file.
	reply=(`cut -c '1-17' ~/.lftp/bookmarks \
		| sed -e 's/[^a-zA-Z0-9\-]//g' `); 
}
compctl -K c_lftp_bookmarks lftp

function c_ssh_knownhosts {
# extract the hosts which are known to ssh.
	reply=(`sed -e 's/ .*$//' \
		    -e 's/,[0-9]\{3\}\..*//' ~/.ssh/known_hosts`);
}
#compctl -K c_ssh_knownhosts ssh

# Complete the current word with files after a `:', with ssh
# hosts after a `@', and with hosts or files (in that order)
# at the start of a word.
compctl -x 'n[0,:]' -f - 'n[0,@],s[]' -K c_ssh_knownhosts -S: + -f --  \
        scp

# This completes `ssh [username@]host remotecommand'.
# TODO: how do we get the -X working?
compctl -x \
	'C[-1,*@*]' -X '<Remote command>' - \
	'n[0,@],s[]' -K c_ssh_knownhosts \
	-- ssh
#compctl -K c_ssh_knownhosts -x 'c[-1,-l]' -k '()' -X 'Remote user name'  -- slogin
#compctl -x 'n[0,@],s[]' -K c_ssh_knownhosts - \
#	'c[-1,-l]' -k '()' -X 'Remote user name' - \
#	'C[-1,*@*]' -k '(kiekeboe)' -- ssh

## As above, but do 'ssh [host] [-l user] [command]' where
## <command> is a separate command line (i.e., to be executed
## via ssh as <user> on <host>).
#compctl -x 'p[1], p[2] C[-1,-*]' -K c_ssh_knownhosts \
#  - 'c[-1,-l]' -K userlist -- \
#  + -x 'w[2,-l] p[4,-1],p[2,-1]' -l '' -- ssh


############################################################################
# Program development

# strip, profile, and debug only executables.  The compctls for the
# debuggers could be better, of course.
compctl -g '*(*)' strip gprof gdb ddd

# GCC completion, based on Andrew Main's; updated for gcc 2.95.2 .
# completes to filenames (*.c, *.C, *.o, etc.); to miscellaneous options after
# a -; to various -f options after -f (and similarly -W, -g and -m); and to a
# couple of other things at different points.
# The -m completion should be tailored to each system; the one below is i386.
compctl -g '*.([cCmisSoa]|cc|cxx|cpp|ii)' -x \
        's[-l]' -s '${(s.:.)^LD_LIBRARY_PATH}/lib*.a(:t:r:s/lib//)' - \
        'c[-1,-x]' -k '(none c objective-c c-header c++ cpp-output assembler ass
embler-with-cpp)' - \
        'c[-1,-o]' -f - \
        'C[-1,-i(nclude|macros)]' -g '*.h' - \
        'C[-1,-i(dirafter|prefix)]' -g '*(-/)' - \
        's[-B][-I][-L]' -g '*(-/)' - \
        's[-fno-],s[-f]' -k '(lang-isoc9x
          allow-single-precision cond-mismatch asm
          builtin freestanding hosted signed-bitfields signed-char
          unsigned-bitfields unsigned-char writable-strings 
          access-control check-new conserve-space dollars-in-identifiers 
          elide-constructors external-templates for-scope gnu-keywords 
          guiding-decls handle-signatures honor-std huge-objects
          implicit-templates init-priority implement-inlines 
          name-mangling-version-N default-inline operator-names optional-diags 
          permissive repo strict-prototype squangle template-depth-N
          this-is-variable vtable-chunks
          syntax-only
          all-virtual dollars-in-identifiers elide-constructors enum-int-equiv 
          memoize-lookups nonnull-objects
          dump-unnumbered pretend-float profile-arcs test-coverage 
	  branch-probabilities optimize-register-moves caller-saves 
          cse-follow-jumps cse-skip-blocks delayed-branch 
          expensive-optimizations fast-math float-store force-addr force-mem 
          data-sections function-sections gcse inline-functions inline-limit-3
          keep-inline-functions default-inline defer-pop function-cse inline 
          peephole omit-frame-pointer regmove rerun-cse-after-loop 
          rerun-loop-opt schedule-insns schedule-insns2 strength-reduce 
          thread-jumps unroll-all-loops unroll-loops move-all-movables 
          reduce-all-givs strict-aliasing
          call-saved- call-used- exceptions fixed- inhibit-size-directive 
          check-memory-usage prefix-function-name common ident gnu-linker 
          pcc-struct-return pic PIC reg-struct-return shared-data short-enums 
          short-double volatile volatile-global volatile-static verbose-asm 
          pack-struct stack-check argument-alias argument-noalias 
          argument-noalias-global leading-underscore)' - \
        's[-g]' -k '(gdb stabs stabs+ coff xcoff xcoff+ dwarf dwarf+ 
          dwarf-2)' - \
        's[-mno-][-mno][-m]' -k '(486 ieee-fp no-fancy-math-387 fp-ret-in-387
          soft-float svr3-shlib no-wide-multiply rtd align-double align-jumps=
          align-loops= align-functions= preferred-stack-boundary=)' - \
        's[-Wno-][-W]' -k '(
          all aggregate-return bad-function-cast cast-align cast-qual
          char-subscripts comment conversion error format id-clash-8
          implicit implicit-int implicit-function-declaration import
          error-implicit-function-declaration inline larger-than-64
          long-long main missing-declarations missing-noreturn
          missing-prototypes multichar nested-externs import parentheses 
          pointer-arith redundant-decls return-type shadow sign-compare 
          strict-prototypes switch traditional trigraphs undef uninitialized 
          unused write-strings unknown-pragmas
          template-debugging 
          ctor-dtor-privacy deprecated effc++ non-template-friend
          non-virtual-dtor old-style-cast overloaded-virtual pmf-conversions
          reorder sign-promo synth)' - \
        's[-]' -k '(pipe ansi traditional traditional-cpp trigraphs pedantic 
          pedantic-errors nostartfiles nostdlib static shared symbolic include 
          imacros idirafter iprefix iwithprefix nostdinc nostdinc++ undef
          print-file-name= print-libgcc-file-name print-prog-name= 
          -print-search-dirs -save-temps)' \
          -X 'Use "-f", "-g", "-m" or "-W" for more options' -- gcc g++

compctl -x 's[--]' -S '' -k '(verbose\  prefix= exec-prefix= help\  build= host=
	norecursion\  program-prefix= program-suffix= program-transform-name= 
	site= srcdir= target= tmpdir= with- without- enable- disable- )' -- \
	./configure

compctl -g '*(/)' \
	-x 's[-]' -P '-' -k '(
	sign clearsign detachsign encrypt symmetric store decrypt verify 
	listkeys listsigs checksigs fingerprint listsecretkeys genkey 
	deletekey editkey genrevoke export import listpackets
	armor localuser remoteuser textmode output verbose batch yes 
	no keyring secretkeyring defaultkey options debug debugall 
	statusfd nocomment completesneeded marginalsneeded 
	loadextension rfc1991 s2kmode s2kdigestalgo s2kcipheralgo 
	cipheralgo digestalgo compressalgo throwkeyid)' -- gpg

# procps
#   ps
#   uptime
compctl -x 'p[1]' -k '(-V)' -- uptime
compctl -x 'p[1]' -k '(-V)' + -k '(-s -d)' -- tload


############################################################################
# Administrative commands

compctl -u -x 'w[2,-c] p[3,-1]' -l '' - \
	's[--]' -s 'fast help login preserve-environment shell version' -- su

#if type rpm >/dev/null; then
#function pkg_glob () {
#    if [[ ${REDHAT_PACKAGES:-0} = 0 ]]; then
#        export REDHAT_PACKAGES="`rpm -qa`"
#    fi
#    reply=(${=REDHAT_PACKAGES})
#}
#function refreshpkgcache () {
#    export REDHAT_PACKAGES="`rpm -qa`"
#}
#compctl -f -x \
#    'c[-1,--root]' -g '*(/)' - \
#    'W[1,-(-install|i*)] s[--]' -k (hash percent force test replacepkgs replacefiles search root) - \
#    'W[1,-(-upgrade|u*)] s[--]' -k (hash percent force test search oldpackage root) - \
#    'C[-1,--(install|upgrade)] s[-],p[1] s[-i],p[1] s[-U]' -X "(h)ash (v)erbose" - \
#    'W[1,-(-erase|e|-verify|V|y)] s[--]' -k (root) - \
#    'W[1,-(-erase|e)]' -K pkg_glob - \
#    'w[1,--query] s[-],p[1] s[-q],p[1] s[-V],p[1] s[-y]' -X \
#"(a)ll (f)ile+ (F)ile-stdin (p)kg+ (P)kg-Stdin \
#(i)nfo (l)ist-of-files (s)tates-of-files (d)ocumentation (c)onfiguration" - \
#    'w[1,--verify] s[-],p[1] s[-V],p[1] s[-y]' -X \
#"(a)ll (f)ile+ (F)ile-stdin (p)kg+ (P)kg-Stdin" - \
#    'W[1,-(-verify|-query|q*|V*|y*)] C[-1,*f]' -f - \
#    'W[1,-(-verify|-query|q*|V*|y*)] C[-1,*p*]' -g '*(D-/) *.rpm(N)' - \
#    'W[1,-(-verify|-query|q*|V*|y*)]' -K pkg_glob - \
#    'W[1,--(where|checksig)]' -K pkg_glob - \
#    'W[1,--rebuild] s[-]' -k (v) - \
#    'p[1] s[-b]' -X "(p)rep (l)ist (c)ompile (i)install (b)inary (a)ll" - \
#    'p[2] C[-1,-b*] s[--]' -k (short-circuit clean sign keep-temps test time-check) - \
#    's[--]' -k (help version install upgrade query verify erase rebuild where checksig) - \
#    's[-]' -k (- i U q V e b) - \
#    'c[-1,rpm]' -X "$(rpm)" -- rpm
#fi #if rpm exists


compctl -k '(if of conv ibs obs bs cbs files skip file seek count)' \
        -S '=' -x 's[if=], s[of=]' -f - 'C[0,conv=*,*] n[-1,,], s[conv=]' \
        -k '(ascii ebcdic ibm block unblock lcase ucase swap noerror sync)' \
        -q -S ',' - 'n[-1,=]' -X ''  -- dd


############################################################################
# Debian specific
source "/usr/share/doc/zsh/examples/compctl.dpkg"

compctl -x 'c[-1,-x]' -g '*.dsc' + -g '*(/)' - \
	   'c[-1,-b]' -g '*(/)' -- dpkg-source
compctl -g '*.deb' + -g '*(/)' lintian
compctl -g '*.changes' + -g '*(/)' dupload

#compctl \
#	-x 'p[1]s[--]' \
#		-k '(setup-lab remove-lab check check-part unpack remove \
#		     help verbose version debug)' -- \
#	+ -x 's[--]' -k '(help verbose version debug)' -- \
#	+ -g '*.deb' + -g '*(/)' \
#	lintian


