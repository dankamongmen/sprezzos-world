set -e

test -x /usr/bin/module-assistant || apt-get install module-assistant

cd /usr/src

kernels="$(ls -d1 linux-headers* | grep -v common | sed -e s/linux-headers-//)"
modules="$(ls -d1 nvidia*.tar.bz2 | sed -e s/.tar.bz2//)"

module-assistant clean $modules
module-assistant build --text-mode --force --kvers-list "$kernels" $modules

ls -l *.deb
for m in $modules ; do
	for k in $kernels ; do
		echo "* ${m}-${k}:"
		ls -l ${m}-${k}_*.deb
	done
done
