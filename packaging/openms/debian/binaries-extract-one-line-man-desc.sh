#!/bin/sh -e

binDir="build/bin"

cd ${binDir}

manPage="../../topp.1"
debLinkFile="../../topp.links"

# Start fresh
rm -f ${debLinkFile}

# List all files in here, but not the Tutorial* binaries.
# Add a dot to the end of each line, where missing.

# First cat the top part of the man page to a temp file

cat ../../topp.1-top-skel > ${manPage}

# Now make the one-liners appear after the top.  Note that each time a
# new binary is dealt with, we create a corresponding link in
# ${debLinkFile}, as each binary must have a corresponding man page.

for file in $(find * -type f -executable -print | grep -v ^Tutorial)
do
    # The man page itself
    oneLiner=$(./$file --help 2>&1 | grep "${file} --" | sed "s|\([^\.]$\)|\1.|g")
    formattedOneLiner=$(echo "${oneLiner}" | sed 's|\(^[[:alnum:]]\+\)|\\\\fB\1\\\\fR|g')
    echo ${formattedOneLiner} >> ${manPage}
    echo "" >> ${manPage}

    # The symbolic link
    echo "usr/share/man/man1/topp.1 usr/share/man/man1/${file}.1" >> ${debLinkFile}
done

# Finally make the bottom of the page.

cat ../../topp.1-bottom-skel >> ${manPage}

cd - > /dev/null 2>&1
