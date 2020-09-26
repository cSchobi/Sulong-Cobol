SUBDIRS = libvbisam bin tests
ACLOCAL_AMFLAGS = -I m4

pkgincludedir = $(includedir)
pkginclude_HEADERS = vbdecimal.h vbisam.h vbisam_rename.h

EXTRA_DIST= cpucheck.c libvbisam/iscommon.c configcit.sh

dist-hook:
	find $(distdir) -type d -print | xargs chmod 777
	chmod 644 $(distdir)/cpucheck.c
	chmod 644 $(distdir)/AUTHORS
	chmod 644 $(distdir)/COPYING
	chmod 644 $(distdir)/COPYING.LIB
	chmod 644 $(distdir)/INSTALL
	chmod 644 $(distdir)/NEWS
	chmod 644 $(distdir)/README
	chmod 755 $(distdir)/ltmain.sh
	chmod 755 $(distdir)/missing
	chmod 755 $(distdir)/install-sh
	chmod 755 $(distdir)/config.guess
	chmod 755 $(distdir)/config.sub
	chmod 755 $(distdir)/depcomp
	chmod 755 $(distdir)/mkinstalldirs
	chmod 755 $(distdir)/configure


