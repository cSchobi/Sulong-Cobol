#                                                       -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2005-2007 Roger While
# Copyright (C) 2008 Cobol-IT
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

AC_PREREQ(2.61)

AC_INIT([VBISAM],[2.1.1],[support@cobol-it.com],[vbisam])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_MACRO_DIR([m4])

AC_MSG_CHECKING(if --enable-debug option specified)
AC_ARG_ENABLE(debug,
	[AC_HELP_STRING([--enable-debug],
			[Build a debugging version.])],
	[cit_debug="$enable_debug"], [cit_debug="no"])
AC_MSG_RESULT($cit_debug)

# Set CFLAGS/CXXFLAGS.  We MUST set the flags before we call autoconf
# compiler configuration macros, because if we don't, they set CFLAGS
# to no optimization and -g, which isn't what we want.
CFLAGS=${CFLAGS-""}
CXXFLAGS=${CXXFLAGS-"$CFLAGS"}
LDFLAGS=${LDFLAGS-""}

# Note for SUN Solaris (gcc)
# export/setenv CFLAGS "-m64 -mptr64"
# export/setenv LDFLAGS "-m64 -mptr64 -L/usr/local/lib/sparcv9"
# ./configure --libdir=/usr/local/lib/sparcv9
#

# Hack for AIX 64 bit
AIX64=0
# Hack for AIX 64 bit (gcc)
# Required -
# export/setenv CFLAGS=-maix64
# export/setenv LDFLAGS=-maix64
if echo $CFLAGS | grep 'aix64' 1>/dev/null 2>&1
then
	AIX64=1
fi
# Hack for AIX 64 bit (xlc)
# Required -
# export/setenv CFLAGS=-q64
# export/setenv LDFLAGS=-q64
if echo $CFLAGS | grep 'q64' 1>/dev/null 2>&1
then
	AIX64=1
fi
if test "$AIX64" = "1" 
then
	if test -f /usr/ccs/bin/ar
	then
		AR="/usr/ccs/bin/ar -X64"
	else
		AR="ar -X64"
	fi
	NM="/usr/ccs/bin/nm -X64 -B"
fi

AM_INIT_AUTOMAKE([gnu no-texinfo.tex])
AM_MAINTAINER_MODE

# Autoheader templates
AH_TEMPLATE([ISAMMODE],   [ISAM mode 0=C-ISAM compatible, 1=Extended])
AH_TEMPLATE([WITH_LFS64], [Compile with large file system I/O])
AH_TEMPLATE([VBDEBUG],   [Turn on debugging mode])
AH_TEMPLATE([__USE_STRING_INLINES], [Compiler optimization])
AH_TEMPLATE([HAS_INLINE], [Can use inline keyword])
AH_TEMPLATE([HAVE__THREAD_ATTR], [Can use __THREAD keyword])
AH_TEMPLATE([HAVE_PTHREAD_H], [Have usable libpthread])

AC_DEFINE(__USE_STRING_INLINES)

AC_ARG_WITH(lfs64,
  [  --with-lfs64            (VBISAM) use large file system for file I/O (default)],
  [ if test "$with_lfs64" = "yes"
    then
	AC_DEFINE(WITH_LFS64)
    fi ],
  [ AC_DEFINE(WITH_LFS64) ])

USE_DEBUG=$cit_debug
AC_ARG_WITH(debug,
  [  --with-debug            (VBISAM) Turn on debugging mode],
  [ USE_DEBUG="yes"
    AC_DEFINE(VBDEBUG) ])

USE_MODE1="yes"
AC_ARG_WITH(compatcisam,
  [  --with-compatcisam      (VBISAM) C-ISAM compatible mode],
  [ USE_MODE1="no" ])

if test "${USE_MODE1}" = "no"
then
	AC_DEFINE(ISAMMODE, 0)
else
	AC_DEFINE(ISAMMODE, 1)
	AC_DEFINE(WITH_LFS64)
fi

AC_MSG_CHECKING(--enable-thread-safe)
AC_ARG_ENABLE(thread-safe,
	[AC_HELP_STRING([--enable-thread-safe],
			[Build Thread safe code.])],
	[CIT_THREAD_SAFE="yes"
	CFLAGS="$CFLAGS -D_REENTRANT"], [CIT_THREAD_SAFE="no"])
AC_MSG_RESULT($CIT_THREAD_SAFE)

CIT_PTHREAD="no"
AC_MSG_CHECKING(--with-pthread)
AC_ARG_WITH(pthread,
	[AC_HELP_STRING([--with-pthread],
			[Use pthread in staid of __thread])],
	[CIT_PTHREAD="yes"], [CIT_PTHREAD="no"])
AC_MSG_RESULT($CIT_PTHREAD)

# Checks for programs.
AC_PROG_CC(gcc xlc cc)
AC_PROG_CPP
AM_PROG_CC_C_O
AC_PROVIDE([AC_PROG_F77])
AC_PROVIDE([AC_PROG_CXX])
with_tags=
AC_PROG_LN_S
AC_PROG_INSTALL
# AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_PROG_MAKE_SET
AC_CHECK_TOOLS(AR, ar, :)

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h malloc.h locale.h stddef.h wchar.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_BIGENDIAN
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_C_INLINE
if test "$ac_cv_c_inline" != no ; then
	AC_DEFINE(HAS_INLINE,1)
fi

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([memmove memset regcomp pow setlocale fcntl strerror \
		strcasecmp strchr strrchr strdup strstr strtol])

AC_CACHE_CHECK([for long long], ac_cv_type_long_long,
	[AC_TRY_LINK([long long ll = 1LL; int i = 63;],
			[long long llmax = (long long) -1;
				return ll << i | ll >> i | llmax / ll | llmax % ll;],
			ac_cv_type_long_long=yes,
			ac_cv_type_long_long=no)])
if test $ac_cv_type_long_long = yes; then
	AC_DEFINE(HAVE_LONG_LONG, 1,
	[Define if you have the 'long long' type.])
fi

if test "$USE_DEBUG" = "no"
then
	CFLAGS=`echo "$CFLAGS " | sed -e 's/-g //'`
	if test "`basename $CC`" = "gcc"
	then
		vb_temp_flags="`${CC} ${srcdir}/cpucheck.c -o cpucheck 1>/dev/null 2>&1 && ./cpucheck 2>/dev/null`"
		rm -f cpucheck${EXEEXT}
		if test "x$vb_temp_flags" != "x"
		then
			CFLAGS="$CFLAGS $vb_temp_flags -finline-functions -fomit-frame-pointer"
		else
			CFLAGS="$CFLAGS -finline-functions -fomit-frame-pointer"
		fi
	fi
fi

AH_VERBATIM([_XOPEN_SOURCE_EXTENDED],
[/* Define to 1 if on HPUX.  */
#ifndef _XOPEN_SOURCE_EXTENDED
# undef _XOPEN_SOURCE_EXTENDED
#endif])dnl

COB_OPTIMIZE_FLAG=${OPTIMIZE_FLAG-"-O"}
case $host_os in
  hpux*)
	if test "`basename $CC`" != "gcc"
	then
		COB_OPTIMIZE_FLAG=${OPTIMIZE_FLAG-"+O2"}
		COB_SHARED_OPT="-b"
		AC_DEFINE([_XOPEN_SOURCE_EXTENDED],1)
		rm -f hptest*
		echo 'int main() { return 0; }' > hptest.c
		${CC} ${CFLAGS} +Olit=all -o hptest hptest.c > hptest.out 2>&1
		if test $? -ne 0 -o -s hptest.out
		then
			CFLAGS="$CFLAGS +ESlit"
		else
			CFLAGS="$CFLAGS +Olit=all"
		fi
		rm -f hptest*
	fi
	;;
  solaris*)
	if test "`basename $CC`" != "gcc"
	then
		COB_OPTIMIZE_FLAG=${OPTIMIZE_FLAG-"-xO2"}		
		COB_CFLAGS="$COB_CFLAGS -xstrconst -w"
		COB_SHARED_OPT="-G"
	fi
	;;
esac

# If the user wants a debugging environment, change any compiler optimization
# flags to -g.  We used to add -g to the -O compiler flags, but compilers are
# good enough at code re-organization that debugging with -O no longer works.
# If you want to compile with a different set of flags, specify CFLAGS in the
# environment before configuring.
if test "$cit_debug" = "yes"; then
	AC_DEFINE(DEBUG)
	AH_TEMPLATE(DEBUG, [Define to 1 if you want a debugging version.])
	CFLAGS="$CFLAGS -g"
else
	CFLAGS="$CFLAGS $COB_OPTIMIZE_FLAG"
fi


if test "`basename $CC`" = "gcc"
then
	CFLAGS="$CFLAGS -fsigned-char -Wall -Wwrite-strings -Wmissing-prototypes -Wno-format-y2k"
fi

# Checks __thread 
if test "$CIT_THREAD_SAFE" = "yes"; then
if test "$CIT_PTHREAD" = "no"; then
 AC_MSG_CHECKING(support __thread)
 AC_LINK_IFELSE([AC_LANG_PROGRAM([[__thread int mythreadvar;]], [[
	mythreadvar = 0;
	return mythreadvar;
	]])],
	[AC_DEFINE(HAVE__THREAD_ATTR)],
	[CIT_THREAD_SAFE="no"
	 CIT_PTHREAD = "yes"]
	)

AC_MSG_RESULT(${CIT_THREAD_SAFE})
fi

if test "$CIT_PTHREAD" = "yes"; then
 AC_MSG_CHECKING(support libpthread)
 AC_CHECK_HEADERS([pthread.h], [AC_CHECK_LIB([pthread], [pthread_setspecific], 
				                         [AC_DEFINE(HAVE_PTHREAD_H)
                                          LDFLAGS="$LDFLAGS -lpthread"], 
	                                     [CIT_THREAD_SAFE="no"]) ],
			       [CIT_THREAD_SAFE="no"])
 AC_MSG_RESULT(${CIT_THREAD_SAFE})
fi
fi

AC_CONFIG_FILES([Makefile libvbisam/Makefile bin/Makefile tests/atlocal tests/Makefile])
AC_OUTPUT

echo ""
echo "VBISAM Configuration:"
echo ""
echo "  CC                             ${CC}"
echo "  CFLAGS                         ${CFLAGS}"
if test "${USE_MODE1}" = "yes"
then
echo "  C-ISAM compatible mode         no"
else
echo "  C-ISAM compatible mode         yes"
fi
echo "  Debugging mode                 ${USE_DEBUG}"
echo "  Thread safe                    ${CIT_THREAD_SAFE}"
echo ""
