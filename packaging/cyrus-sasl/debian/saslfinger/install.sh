#!/bin/bash

scriptname="saslfinger"
man_paths=(/usr/share/man)

if [ ! -z "$DESTDIR" ] ; then
        echo "DESTDIR ($DESTDIR) found in the environment. Will be used as install prefix."
fi

# verify_man_page ()
# Check if the man page for this script has been installed and install it
# if it isn't there.

verify_man_page ()
{
for man_path in ${man_paths[@]}
do
local man_source="${scriptname}.1"
local man_dest="$DESTDIR${man_path}/man1/${scriptname}.1"
if ! [[ -e ${man_dest} ]]; then
	echo "Installing man page..."
	$(cp ${man_source} ${man_dest})
elif [[ ${man_dest} -ot ${man_source} ]]; then
	echo "Updating ${scriptname} man page..."
	$(cp ${man_source} ${man_dest})
else
	echo "${scriptname} man page is up to date. Nothing to do."
fi
done
}


verify_script ()
{
local source_dir="${scriptname}"
local install_dir="$DESTDIR/usr/bin/${scriptname}"
if ! [[ -e ${install_dir} ]]; then
	echo "Installing ${scriptname}..."
	`cp ${source_dir} ${install_dir}`
	`chmod 755 ${install_dir}`
elif [[ ${install_dir} -ot ${source_dir} ]]; then
	echo "Updating ${scriptname}..."
	`cp -p -f ${source_dir} ${install_dir}`
	`chmod 755 ${install_dir}`
else
	echo "${scriptname} is up to date. Nothing to do."
fi
}

verify_script
verify_man_page
exit 0
