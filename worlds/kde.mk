.PHONY: akonadi
akonadi:$(AKONADI)_$(ARCH).deb
$(AKONADI): $(SPREZZ)/akonadi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf akonadi-$(akonadi_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: amarok
amarok:$(AMAROK)_$(ARCH).deb
$(AMAROK): $(SPREZZ)/amarok/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf amarok-$(amarok_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: attica
attica:$(ATTICA)_$(ARCH).deb
$(ATTICA): $(SPREZZ)/attica/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf attica-$(attica_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: bluedevil
bluedevil:$(BLUEDEVIL)_$(ARCH).deb
$(BLUEDEVIL): $(SPREZZ)/bluedevil/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf bluedevil-$(bluedevil_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: calligra
calligra:$(CALLIGRA)_$(ARCH).deb
$(CALLIGRA): $(SPREZZ)/calligra/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf calligra-$(calligra_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: digikam
digikam:$(DIGIKAM)_$(ARCH).deb
$(DIGIKAM): $(SPREZZ)/digikam/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf digikam-$(digikam_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: filelight
filelight:$(FILELIGHT)_$(ARCH).deb
$(FILELIGHT): $(SPREZZ)/filelight/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf filelight_$(filelight_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: fotowall
fotowall:$(FOTOWALL)_$(ARCH).deb
$(FOTOWALL): $(SPREZZ)/fotowall/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf Fotowall-$(fotowall_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: grantlee
grantlee:$(GRANTLEE)_$(ARCH).deb
$(GRANTLEE): $(SPREZZ)/grantlee/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf grantlee-$(grantlee_UPVER).tar.gz $(TARARGS) $@

.PHONY: gwenview
gwenview:$(GWENVIEW)_$(ARCH).deb
$(GWENVIEW): $(SPREZZ)/gwenview/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xJvf gwenview-$(gwenview_UPVER).tar.xz $(TARARGS) $@

.PHONY: hupnp
hupnp:$(HUPNP)_$(ARCH).deb
$(HUPNP): $(SPREZZ)/hupnp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf hupnp_$(hupnp_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: k3b
k3b:$(K3B)_$(ARCH).deb
$(K3B): $(SPREZZ)/k3b/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf k3b-$(k3b_UPVER).tar.gz $(TARARGS) $@

.PHONY: kaccessible
kaccessible:$(KACCESSIBLE)_$(ARCH).deb
$(KACCESSIBLE): $(SPREZZ)/kaccessible/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kaccessible-$(kaccessible_UPVER).tar.xz $(TARARGS) $@

.PHONY: kactivities
kactivities:$(KACTIVITIES)_$(ARCH).deb
$(KACTIVITIES): $(SPREZZ)/kactivities/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kactivities_$(kactivities_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kate
kate:$(KATE)_$(ARCH).deb
$(KATE): $(SPREZZ)/kate/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kate-$(kate_UPVER).tar.xz $(TARARGS) $@

.PHONY: kde4libs
kde4libs:$(KDE4LIBS)_$(ARCH).deb
$(KDE4LIBS): $(SPREZZ)/kde4libs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde4libs_$(kde4libs_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kdeartwork
kdeartwork:$(KDEARTWORK)_$(ARCH).deb
$(KDEARTWORK): $(SPREZZ)/kdeartwork/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdeartwork_$(kdeartwork_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kde-baseapps
kde-baseapps:$(KDEBASEAPPS)_$(ARCH).deb
$(KDEBASEAPPS): $(SPREZZ)/kde-baseapps/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde-baseapps_$(kde-baseapps_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kdemultimedia
kdemultimedia:$(KDEMULTIMEDIA)_$(ARCH).deb
$(KDEMULTIMEDIA): $(SPREZZ)/kdemultimedia/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdemultimedia-$(kdemultimedia_UPVER).tar.xz $(TARARGS) $@

.PHONY: kdenetwork
kdenetwork:$(KDENETWORK)_$(ARCH).deb
$(KDENETWORK): $(SPREZZ)/kdenetwork/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdenetwork-$(kdenetwork_UPVER).tar.xz $(TARARGS) $@

.PHONY: kdepim-runtime
kdepim-runtime:$(KDEPIMRUNTIME)_$(ARCH).deb
$(KDEPIMRUNTIME): $(SPREZZ)/kdepim-runtime/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdepim-runtime-$(kdepim-runtime_UPVER).tar.xz $(TARARGS) $@

.PHONY: kdepimlibs
kdepimlibs:$(KDEPIMLIBS)_$(ARCH).deb
$(KDEPIMLIBS): $(SPREZZ)/kdepimlibs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdepimlibs_$(kdepimlibs_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kdesdk
kdesdk:$(KDESDK)_$(ARCH).deb
$(KDESDK): $(SPREZZ)/kdesdk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdesdk-$(kdesdk_UPVER).tar.xz $(TARARGS) $@

.PHONY: kdevelop
kdevelop:$(KDEVELOP)_$(ARCH).deb
$(KDEVELOP): $(SPREZZ)/kdevelop/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf kdevelop-$(kdevelop_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: kdevplatform
kdevplatform:$(KDEVPLATFORM)_$(ARCH).deb
$(KDEVPLATFORM): $(SPREZZ)/kdevplatform/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf kdevplatform-$(kdevplatform_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: kde-gtk-config
kde-gtk-config:$(KDEGTKCONFIG)_$(ARCH).deb
$(KDEGTKCONFIG): $(SPREZZ)/kde-gtk-config/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf kde-gtk-config-$(kde-gtk-config_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: kde-runtime
kde-runtime:$(KDERUNTIME)_$(ARCH).deb
$(KDERUNTIME): $(SPREZZ)/kde-runtime/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde-runtime-$(kde-runtime_UPVER).tar.xz $(TARARGS) $@

.PHONY: kde-style-qtcurve
kde-style-qtcurve:$(KDESTYLEQTCURVE)_$(ARCH).deb
$(KDESTYLEQTCURVE): $(SPREZZ)/kde-style-qtcurve/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf QtCurve-KDE4-$(kde-style-qtcurve_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: kde-wallpapers
kde-wallpapers:$(KDEWALLPAPERS)_$(ARCH).deb
$(KDEWALLPAPERS): $(SPREZZ)/kde-wallpapers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde-wallpapers-$(kde-wallpapers_UPVER).tar.xz $(TARARGS) $@

.PHONY: kde-workspace
kde-workspace:$(KDEWORKSPACE)_$(ARCH).deb
$(KDEWORKSPACE): $(SPREZZ)/kde-workspace/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde-workspace_$(kde-workspace_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kgamma
kgamma:$(KGAMMA)_$(ARCH).deb
$(KGAMMA): $(SPREZZ)/kgamma/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kgamma-$(kgamma_UPVER).tar.xz $(TARARGS) $@

.PHONY: konsole
konsole:$(KONSOLE)_$(ARCH).deb
$(KONSOLE): $(SPREZZ)/konsole/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf konsole-$(konsole_UPVER).tar.xz $(TARARGS) $@

.PHONY: kvirc
kvirc:$(KVIRC)_$(ARCH).deb
$(KVIRC): $(SPREZZ)/kvirc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf kvirc-$(kvirc_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libbluedevil
libbluedevil:$(LIBBLUEDEVIL)_$(ARCH).deb
$(LIBBLUEDEVIL): $(SPREZZ)/libbluedevil/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libbluedevil-$(libbluedevil_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libdbusmenu-qt
libdbusmenu-qt:$(LIBDBUSMENUQT)_$(ARCH).deb
$(LIBDBUSMENUQT): $(SPREZZ)/libdbusmenu-qt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libdbusmenu-qt-$(libdbusmenu-qt_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libkate
libkate:$(LIBKATE)_$(ARCH).deb
$(LIBKATE): $(SPREZZ)/libkate/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libkate-$(libkate_UPVER).tar.gz $(TARARGS) $@

.PHONY: marble
marble:$(MARBLE)_$(ARCH).deb
$(MARBLE): $(SPREZZ)/marble/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf marble-$(marble_UPVER).tar.xz $(TARARGS) $@

.PHONY: nas
nas:$(NAS)_$(ARCH).deb
$(NAS): $(SPREZZ)/nas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nas-$(nas_UPVER).src.tar.gz $(TARARGS) $@

.PHONY: nepomukcore
nepomukcore:$(NEPOMUKCORE)_$(ARCH).deb
$(NEPOMUKCORE): $(SPREZZ)/nepomukcore/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf nepomuk-core-$(nepomukcore_UPVER).tar.xz $(TARARGS) $@

.PHONY: oxygen-icons
oxygen-icons:$(OXYGENICONS)_$(ARCH).deb
$(OXYGENICONS): $(SPREZZ)/oxygen-icons/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf oxygen-icons-$(oxygen-icons_UPVER).tar.xz $(TARARGS) $@

.PHONY: phonon
phonon:$(PHONON)_$(ARCH).deb
$(PHONON): $(SPREZZ)/phonon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf phonon-$(phonon_UPVER).tar.xz $(TARARGS) $@

.PHONY: qimageblitz
qimageblitz:$(QIMAGEBLITZ)_$(ARCH).deb
$(QIMAGEBLITZ): $(SPREZZ)/qimageblitz/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qimageblitz-$(qimageblitz_UPVER).tar.gz $(TARARGS) $@

.PHONY: qscintilla2
qscintilla2:$(QSCINTILLA2)_$(ARCH).deb
$(QSCINTILLA2): $(SPREZZ)/qscintilla2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf QScintilla-gpl-$(qscintilla2_UPVER).tar.gz $(TARARGS) $@

.PHONY: qtmobility
qtmobility:$(QTMOBILITY)_$(ARCH).deb
$(QTMOBILITY): $(SPREZZ)/qtmobility/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qt-mobility-opensource-src-$(qtmobility_UPVER).tar.gz $(TARARGS) $@

.PHONY: qtzeitgeist
qtzeitgeist:$(QTZEITGEIST)_$(ARCH).deb
$(QTZEITGEIST): $(SPREZZ)/qtzeitgeist/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf QtZeitgeist-$(qtzeitgeist_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: qt-assistant-compat
qt-assistant-compat:$(QTASSISTANTCOMPAT)_$(ARCH).deb
$(QTASSISTANTCOMPAT): $(SPREZZ)/qt-assistant-compat/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qt-assistant-qassistantclient-library-compat-src-$(qt-assistant-compat_UPVER).tar.gz $(TARARGS) $@

.PHONY: qt4-x11
qt4-x11:$(QT4X11)_$(ARCH).deb
$(QT4X11): $(SPREZZ)/qt4-x11/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qt-everywhere-opensource-src-$(qt4-x11_UPVER).tar.gz $(TARARGS) $@

.PHONY: qt5-x11
qt5-x11:$(QT5X11)_$(ARCH).deb
$(QT5X11): $(SPREZZ)/qt5-x11/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qt-everywhere-opensource-src-$(qt5-x11_UPVER).tar.gz $(TARARGS) $@

.PHONY: qtwebkit
qtwebkit:$(QTWEBKIT)_$(ARCH).deb
$(QTWEBKIT): $(SPREZZ)/qtwebkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qtwebkit-$(qtwebkit_UPVER).tar.gz $(TARARGS) $@

.PHONY: qwt
qwt:$(QWT)_$(ARCH).deb
$(QWT): $(SPREZZ)/qwt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf qwt-$(qwt_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: skrooge
skrooge:$(SKROOGE)_$(ARCH).deb
$(SKROOGE): $(SPREZZ)/skrooge/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf skrooge-$(skrooge_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: simon
simon:$(SIMON)_$(ARCH).deb
$(SIMON): $(SPREZZ)/simon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf simon-$(simon_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: soprano
soprano:$(SOPRANO)_$(ARCH).deb
$(SOPRANO): $(SPREZZ)/soprano/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf soprano-$(soprano_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: strigi
strigi:$(STRIGI)_$(ARCH).deb
$(STRIGI): $(SPREZZ)/strigi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf strigi-$(strigi_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: yakuake
yakuake:$(YAKUAKE)_$(ARCH).deb
$(YAKUAKE): $(SPREZZ)/yakuake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf yakuake-$(yakuake_UPVER).tar.bz2 $(TARARGS) $@
.PHONY: qjson
qjson:$(QJSON)_$(ARCH).deb
$(QJSON): $(SPREZZ)/qjson/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf qjson-$(qjson_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: nepomukwidgets
nepomukwidgets:$(NEPOMUKWIDGETS)_$(ARCH).deb
$(NEPOMUKWIDGETS): $(SPREZZ)/nepomukwidgets/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf nepomuk-widgets-$(nepomukwidgets_UPVER).tar.xz $(TARARGS) $@

.PHONY: libkipi
libkipi:$(LIBKIPI)_$(ARCH).deb
$(LIBKIPI): $(SPREZZ)/libkipi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libkipi-$(libkipi_UPVER).tar.xz $(TARARGS) $@

#.PHONY: qmf
#qmf:$(QMF)_$(ARCH).deb
#$(QMF): $(SPREZZ)/qmf/debian/changelog
#	mkdir $@
#	cp -r $(<D) $@
#	cd $@ && uscan --force-download --download-current-version
#	tar xzvf qmf-$(qmf_UPVER).tar.gz $(TARARGS) $@

.PHONY: qmf
qmf:$(QMF)_$(ARCH).deb
$(QMF): $(SPREZZ)/qmf/debian/changelog
	git clone git://gitorious.org/qt-labs/messagingframework.git $@
	rm -rf $@/debian
	tar cJf qmf-$(qmf_UPVER).tar.xz $@ --exclude-vcs
	ln -sf qmf-$(qmf_UPVER).tar.xz qmf_$(qmf_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: polkit-qt-1
polkit-qt-1:$(POLKITQT1)_$(ARCH).deb
$(POLKITQT1): $(SPREZZ)/polkit-qt-1/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf polkit-qt-1-$(polkit-qt-1_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: srtp
srtp:$(SRTP)_$(ARCH).deb
$(SRTP): $(SPREZZ)/srtp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf srtp-$(srtp_UPVER).tar.gz $(TARARGS) $@

.PHONY: kino
kino:$(KINO)_$(ARCH).deb
$(KINO): $(SPREZZ)/kino/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf kino_$(kino_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pykde4
pykde4:$(PYKDE4)_$(ARCH).deb
$(PYKDE4): $(SPREZZ)/pykde4/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf pykde4_$(pykde4_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kdocker
kdocker:$(KDOCKER)_$(ARCH).deb
$(KDOCKER): $(SPREZZ)/kdocker/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf kdocker_$(kdocker_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: lightdm-kde
lightdm-kde:$(LIGHTDMKDE)_$(ARCH).deb
$(LIGHTDMKDE): $(SPREZZ)/lightdm-kde/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf lightdm-kde_$(lightdm-kde_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: svgpart
svgpart:$(SVGPART)_$(ARCH).deb
$(SVGPART): $(SPREZZ)/svgpart/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf svgpart_$(svgpart_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kdepim
kdepim:$(KDEPIM)_$(ARCH).deb
$(KDEPIM): $(SPREZZ)/kdepim/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf kdepim_$(kdepim_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: kwallet
kwallet:$(KWALLET)_$(ARCH).deb
$(KWALLET): $(SPREZZ)/kwallet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kwallet_$(kwallet_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: libksane
libksane:$(LIBKSANE)_$(ARCH).deb
$(LIBKSANE): $(SPREZZ)/libksane/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libksane_$(libksane_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: apper
apper:$(APPER)_$(ARCH).deb
$(APPER): $(SPREZZ)/apper/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf apper_$(apper_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: listaller
listaller:$(LISTALLER)_$(ARCH).deb
$(LISTALLER): $(SPREZZ)/listaller/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf listaller_$(listaller_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libkexiv2
libkexiv2:$(LIBKEXIV2)_$(ARCH).deb
$(LIBKEXIV2): $(SPREZZ)/libkexiv2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libkexiv2_$(libkexiv2_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: ksnapshot
ksnapshot:$(KSNAPSHOT)_$(ARCH).deb
$(KSNAPSHOT): $(SPREZZ)/ksnapshot/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf ksnapshot_$(ksnapshot_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: libqtgtl
libqtgtl:$(LIBQTGTL)_$(ARCH).deb
$(LIBQTGTL): $(SPREZZ)/libqtgtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libqtgtl_$(libqtgtl_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: smb4k
smb4k:$(SMB4K)_$(ARCH).deb
$(SMB4K): $(SPREZZ)/smb4k/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf smb4k_$(smb4k_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: okular
okular:$(OKULAR)_$(ARCH).deb
$(OKULAR): $(SPREZZ)/okular/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf okular_$(okular_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: smokekde
smokekde:$(SMOKEKDE)_$(ARCH).deb
$(SMOKEKDE): $(SPREZZ)/smokekde/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf smokekde_$(smokekde_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: smokegen
smokegen:$(SMOKEGEN)_$(ARCH).deb
$(SMOKEGEN): $(SPREZZ)/smokegen/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf smokegen_$(smokegen_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: smokeqt
smokeqt:$(SMOKEQT)_$(ARCH).deb
$(SMOKEQT): $(SPREZZ)/smokeqt/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf smokeqt_$(smokeqt_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: qwt5
qwt5:$(QWT5)_$(ARCH).deb
$(QWT5): $(SPREZZ)/qwt5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf qwt5_$(qwt5_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: kamera
kamera:$(KAMERA)_$(ARCH).deb
$(KAMERA): $(SPREZZ)/kamera/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kamera_$(kamera_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: libkdcraw
libkdcraw:$(LIBKDCRAW)_$(ARCH).deb
$(LIBKDCRAW): $(SPREZZ)/libkdcraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libkdcraw_$(libkdcraw_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: qt-gstreamer
qt-gstreamer:$(QTGSTREAMER)_$(ARCH).deb
$(QTGSTREAMER): $(SPREZZ)/qt-gstreamer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qt-gstreamer_$(qt-gstreamer_UPVER).orig.tar.gz $(TARARGS) $@

