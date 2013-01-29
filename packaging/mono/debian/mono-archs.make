DEB_MONO_ARCHS = amd64 armel i386 ia64 kfreebsd-amd64 kfreebsd-i386 powerpc ppc64 s390x sparc
# grep defined mono/metadata/sgen-archdep.h
#elif defined(__x86_64__)
#elif defined(__ppc__)
#elif defined(__arm__)
#elif defined(__s390x__)
# mono with sgen FTBFS on powerpc
DEB_MONO_SGEN_ARCHS = amd64 armel i386 kfreebsd-amd64 kfreebsd-i386 s390x
