noinst_PROGRAMS = mvtest detest00 detest01 jvntest mvtest2
mvtest_SOURCES = mvtest.c
mvtest2_SOURCES = mvtest2.c
detest00_SOURCES = detest00.c
detest01_SOURCES = detest01.c
jvntest_SOURCES = jvntest.c

CLEANFILES = *.idx *.dat RECOVER*
DISTCLEANFILES = atconfig
EXTRA_DIST = TESTDATA

AM_CFLAGS = -I$(top_srcdir) -I$(top_srcdir)/libvbisam
AM_LDFLAGS = -L$(top_builddir)/libvbisam/.libs -lvbisam
