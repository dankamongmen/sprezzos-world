strip_hash(){
	echo "$1" | sed 's/-................................$//'
}

sort_uniq(){
    {
	for i in "$@" ; do
	    echo $i
	done
    } | sort -u | tr "\n" " "
}

dependency(){
    local package
    local version
    local next_upstream_version
    package=$1
    version=`dpkg-query --showformat='${Version}' -W $package`
    next_upstream_version=`echo $version | sed  -e 's/-[^-]*$//' -e 's/$/+/'`
    echo "$package (>= $version), $package (<< $next_upstream_version)"
}

providing_package_for_ghc(){
    local package
    local dep
    local dir
    local dirs
    local lib
    dep=`strip_hash $1`
    dirs=`ghc-pkg field $dep library-dirs | grep -i ^library-dirs | cut -d':' -f 2`
    lib=`ghc-pkg field $dep hs-libraries | grep -i ^hs-libraries |  sed -e 's|hs-libraries: *\([^ ]*\).*|\1|' `
    for dir in $dirs ; do
	if [ -e "${dir}/lib${lib}.a" ] ; then
	    package=`dpkg-query -S ${dir}/lib${lib}.a | cut -d':' -f 1` || exit $?
	    continue
	fi
    done
    echo $package
}

providing_package_for_ghc_prof(){
    local package
    local dep
    local dir
    local dirs
    local lib
    dep=`strip_hash $1`
    dirs=`ghc-pkg field $dep library-dirs | grep -i ^library-dirs | cut -d':' -f 2`
    lib=`ghc-pkg field $dep hs-libraries | grep -i ^hs-libraries | sed -e 's|hs-libraries: *\([^ ]*\).*|\1|' `
    for dir in $dirs ; do
	if [ -e "${dir}/lib${lib}_p.a" ] ; then
	    package=`dpkg-query -S ${dir}/lib${lib}_p.a | cut -d':' -f 1` || exit $?
	    continue
	fi
    done
    echo $package
}

cabal_package_ids(){
    local config
    local package_ids
    until [ -z "$1" ]
    do
      config=$1
      package_ids="$package_ids `grep-dctrl -n -i -s Id "" $config`"
      shift
    done
    echo $package_ids
}

cabal_depends(){
    local config
    local dep
    local depends
    local final_depends
    until [ -z "$1" ]
    do
      config=$1
      depends="$depends `grep-dctrl -n -i -s Depends "" $config | tr "," " "`"
      shift
    done
    for dep in `sort_uniq $depends` ; do
	# The package is not mentioned in the ignored package list with the same version
	# or mentioned without any version in the ignored package list?
	if  echo " $ignores " | grep -qv " $dep " &&
	    echo " $ignores " | grep -qv " `echo $dep | sed s%-[0-9][.0-9a-zA-Z]*$%%` " ;
	then
	    final_depends="$final_depends $dep"
	fi
    done
    echo $final_depends
}

hashed_dependency(){
    local type
    local pkgid
    local virpkg
    type=$1
    pkgid=$2
    virtual_pkg=`package_id_to_virtual_package $type $pkgid`
    # As a transition measure, check if dpkg knows about this virtual package
    if dpkg-query -W $virtual_pkg >/dev/null 2>/dev/null;
    then
   	 echo $virtual_pkg
    fi
}

depends_for_ghc(){
    local dep
    local packages
    local pkgid
    for pkgid in `cabal_depends $@` ; do
	dep=`hashed_dependency dev $pkgid`
	if [ -z "$dep" ]
	then
	  pkg=`providing_package_for_ghc $pkgid`
          if [ -n "$pkg" ]
          then
	      dep=`dependency $pkg`
	      packages="$packages, $dep"
          fi
        else
	    packages="$packages, $dep"
	fi
    done

    echo $packages | sed -e 's/^,[ ]*//'
}

depends_for_ghc_prof(){
    local dep
    local packages
    local pkgid
    for pkgid in `cabal_depends $@` ; do
	dep=`hashed_dependency prof $pkgid`
	if [ -z "$dep" ]
	then
	  pkg=`providing_package_for_ghc_prof $pkgid`
	  dep=`dependency $pkg`
	fi
	packages="$packages, $dep"
    done

    echo $packages | sed -e 's/^,[ ]*//'
}

provides_for_ghc(){
    local dep
    local packages
    for package_id in `cabal_package_ids $@` ; do
	packages="$packages, `package_id_to_virtual_package dev $package_id`"
    done
    echo $packages | sed -e 's/^,[ ]*//'
}

provides_for_ghc_prof(){
    local dep
    local packages
    for package_id in `cabal_package_ids $@` ; do
	packages="$packages, `package_id_to_virtual_package prof $package_id`"
    done
    echo $packages | sed -e 's/^,[ ]*//'
}

package_id_to_virtual_package(){
	local type
	type="$1"
	echo $2 | tr A-Z a-z | \
            grep '[a-z0-9]\+-[0-9\.]\+-................................' | \
		perl -pe 's/([a-z0-9-]+)-([0-9\.]+)-(.....).........................../libghc-\1-'$type'-\2-\3/'
}

depends_for_hugs(){
    local version
    local upstream_version
    version=`dpkg-query --showformat='${Version}' -W hugs`
    upstream_version=`echo $version | sed -e 's/-[^-]*$//'`
    echo "hugs (>= $upstream_version)"
}

find_config_for_ghc(){
    local f
    local pkg
    pkg=$1
    case "$pkg" in
	ghc-prof)
	    pkg=ghc
	    ;;
	libghc-*-prof)
	    pkg=`echo $pkg | sed -e 's/-prof$/-dev/'`
	    ;;
	*)
	    ;;
    esac
    for f in debian/$pkg/var/lib/ghc/package.conf.d/*.conf ; do
	if [ -f "$f" ] ; then
	    echo $f
	    echo " "
	fi
    done
}


if ! [ `which grep-dctrl` > /dev/null ] ; then
    echo "grep-dctrl is missing" >&2
    exit 1
fi

args=
ignores=
files=
until [ -z "$1" ]
do
  case "$1" in
      -X*)
          pkg=${1##-X}
	  ignores="$ignores $pkg"
	  ;;

      --exclude=*)
	  pkg=${1##--exclude=}
	  ignores="$ignores $pkg"
	  ;;

      -*)
	  args="$args $1"
	  ;;
      *)
	  if [ -f $1 ] ; then
	      files="$files $1"
	  else
	      echo "Installed package description file $1 can not be found" >&2
	      exit 1
	  fi
	  ;;
  esac
  shift
done

