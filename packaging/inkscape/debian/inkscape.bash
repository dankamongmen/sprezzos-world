# inkscape(1) completion 
# put this file in /etc/bash_completion.d/ 
# allali@univ-mlv.fr

have inkscape &&
_inkscape()
{
        local cur

        COMPREPLY=()
        cur=${COMP_WORDS[COMP_CWORD]}

        if [[ "$cur" == -* ]]; then
                COMPREPLY=( $( compgen -W '-? --help --usage -V --version \
			-z --without-gui -g --with-gui -f --file= -p --print= \
			-e --export-png= -d --export-dpi= -a --export-area= \
			-w --export-width= -h --export-height= -i --export-id= \
			-j --export-id-only  -t --export-use-hints -b --export-background= \
			-y --export-background-opacity= -l --export-plain-svg= -s --slideshow' -- $cur ) ) 
        else
                _filedir '@(ai|ani|bmp|cur|dia|eps|gif|ggr|ico|jpe|jpeg|jpg|pbm|pcx|pdf|pgm|png|ppm|pnm|ps|ras|sk|svg|svgz|targa|tga|tif|tiff|txt|wbmp|wmf|xbm|xpm)'
        fi

}
[ "${have:-}" ] && complete -F _inkscape $filenames inkscape
