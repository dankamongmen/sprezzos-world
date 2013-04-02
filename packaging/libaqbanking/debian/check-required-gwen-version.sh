#! /bin/sh

builddep_gwen_ver=$(sed -n 's/^.* libgwenhywfar[0-9]\+-dev (>= \([0-9.]\+\).*$/\1/p' debian/control)
for pkg_gwen_ver in $(sed -n 's/^Depends:.* libgwenhywfar[0-9]\+ (>= \([0-9.]\+\).*$/\1/p' debian/*/DEBIAN/control); do
  if dpkg --compare-versions "$pkg_gwen_ver" gt "$builddep_gwen_ver"; then
    cat - <<EOT
WARNING:
A built binary package has a dependency on libgwenhywfar which requires a
higher version than required in build dependencies.  This is a hint that the
build dependency might need a higher version of libgwenhywfar-dev too, which is
probably fulfilled at the moment and thus didn't cause a build failure on THIS
system, but may cause a build failure on OTHER systems.
EOT
    exit 1
  fi
done
