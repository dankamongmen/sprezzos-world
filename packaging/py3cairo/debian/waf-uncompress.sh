#!/bin/sh

BASEDIR=$1
OLD_IFS=$IFS
IFS="
"

script=""
tar_bz2=""

WAF="$BASEDIR/waf"
inputdata=`sed -e "s|\\\\x0|#0000|g" $WAF | sed -e "s|\\\\\\\\|#005c|g"`
is_script=1
for line in $inputdata
do
    case $line in
        \#==\>)
            is_script=0
        ;;
        
        \#\<==)
            is_script=1
        ;;
        
        *)
            if test $is_script -eq 1; then
                script=$script$line"\n"
            else
                tar_bz2=$line
            fi
        ;;
    esac
done
echo "$script" > "$BASEDIR/waf-uncompressed"
chmod +x "$BASEDIR/waf-uncompressed"
echo -n "$tar_bz2" | sed "s|^#||g" | sed "s|#\\/|\\n|g" | sed "s|#\\*|\\r|g" | sed "s|#0000|\\x0|g" | sed "s|#005c|\\\\|g" | \
    tar -xjf - -C $BASEDIR
IFS=$OLD_IFS

