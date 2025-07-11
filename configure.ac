#  Copyright 2005 Adam Jackson.
#
#  Permission is hereby granted, free of charge, to any person obtaining a
#  copy of this software and associated documentation files (the "Software"),
#  to deal in the Software without restriction, including without limitation
#  on the rights to use, copy, modify, merge, publish, distribute, sub
#  license, and/or sell copies of the Software, and to permit persons to whom
#  the Software is furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice (including the next
#  paragraph) shall be included in all copies or substantial portions of the
#  Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.  IN NO EVENT SHALL
#  ADAM JACKSON BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
#  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# Process this file with autoconf to produce a configure script

# Initialize Autoconf
AC_PREREQ([2.60])
AC_INIT([xf86-video-r128],
        [6.13.0],
        [https://gitlab.freedesktop.org/xorg/driver/xf86-video-r128/-/issues],
        [xf86-video-r128])
AC_CONFIG_SRCDIR([Makefile.am])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR(.)

# Initialize Automake
AM_INIT_AUTOMAKE([foreign dist-xz])

# Require X.Org macros 1.8 or later for MAN_SUBSTS set by XORG_MANPAGE_SECTIONS
m4_ifndef([XORG_MACROS_VERSION],
          [m4_fatal([must install xorg-macros 1.8 or later before running autoconf/autogen])])
XORG_MACROS_VERSION(1.8)
XORG_DEFAULT_OPTIONS

# Initialize libtool
LT_INIT([disable-static])

AH_TOP([#include "xorg-server.h"])

# Define a configure option for an alternate module directory
AC_ARG_WITH(xorg-module-dir,
            AS_HELP_STRING([--with-xorg-module-dir=DIR],
                           [Default xorg module directory [[default=$libdir/xorg/modules]]]),
            [moduledir="$withval"],
            [moduledir="$libdir/xorg/modules"])

AC_ARG_ENABLE(dri, AS_HELP_STRING([--disable-dri],
                                  [Disable DRI support [[default=auto]]]),
              [DRI="$enableval"],
              [DRI=auto])

AC_ARG_ENABLE(exa, AS_HELP_STRING([--disable-exa],
				  [Disable EXA support [[default=enabled]]]),
              [EXA="$enableval"],
              [EXA=yes])

# Store the list of server defined optional extensions in REQUIRED_MODULES
XORG_DRIVER_CHECK_EXT(RANDR, randrproto)
XORG_DRIVER_CHECK_EXT(RENDER, renderproto)
XORG_DRIVER_CHECK_EXT(XV, videoproto)
XORG_DRIVER_CHECK_EXT(XF86MISC, xf86miscproto)
XORG_DRIVER_CHECK_EXT(DPMSExtension, xextproto)

# Obtain compiler/linker options for the driver dependencies
PKG_CHECK_MODULES(XORG, [xorg-server >= 1.18 xproto fontsproto $REQUIRED_MODULES])
PKG_CHECK_MODULES(XEXT, [xextproto >= 7.0.99.1],
                  HAVE_XEXTPROTO_71="yes"; AC_DEFINE(HAVE_XEXTPROTO_71, 1, [xextproto 7.1 available]),
                  HAVE_XEXTPROTO_71="no")
AM_CONDITIONAL(HAVE_XEXTPROTO_71, [ test "$HAVE_XEXTPROTO_71" = "yes" ])
sdkdir=`$PKG_CONFIG --variable=sdkdir xorg-server`

# Checks for libraries.

if test "$DRI" != no; then
	if test -f "${sdkdir}/dri.h"; then
		have_dri_h="yes"
	else
		have_dri_h="no"
	fi
	if test -f "${sdkdir}/sarea.h"; then
		have_sarea_h="yes"
	else
		have_sarea_h="no"
	fi
	if test -f "${sdkdir}/dristruct.h"; then
		have_dristruct_h="yes"
	else
		have_dristruct_h="no"
	fi
	if test -f "${sdkdir}/damage.h"; then
		have_damage_h="yes"
	else
		have_damage_h="no"
	fi
fi

AC_MSG_CHECKING([whether to include DRI support])
if test x$DRI = xauto; then
	if test "$have_dri_h" = yes -a \
                "$have_sarea_h" = yes -a \
                "$have_dristruct_h" = yes; then
		DRI="yes"
	else
		DRI="no"
	fi
fi
AC_MSG_RESULT([$DRI])

AM_CONDITIONAL(DRI, test x$DRI = xyes)
if test "$DRI" = yes; then
	PKG_CHECK_MODULES(DRI, [libdrm >= 2.2 xf86driproto])
        AC_DEFINE(R128DRI,1,[Enable DRI driver support])
        AC_DEFINE(R128DRI_DEVEL,1,[Enable developmental DRI driver support])
fi

SAVE_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $XORG_CFLAGS"
# Properly handle EXA.
AC_MSG_CHECKING([whether to enable EXA support])
if test "x$EXA" = xyes; then
        AC_MSG_RESULT(yes)
        AS_IF([test -f "${sdkdir}/exa.h"], [have_exa_h="yes"], [have_exa_h="no"])
else
        AC_MSG_RESULT(no)
fi

SAVE_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $XORG_CFLAGS"
if test "x$have_exa_h" = xyes; then
        AC_MSG_CHECKING([whether EXA version is at least 2.0.0])
        AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include "xorg-server.h"
#include "exa.h"
#if EXA_VERSION_MAJOR < 2
#error OLD EXA!
#endif
                          ]])],
                          [USE_EXA=yes],
                          [USE_EXA=no])
        AC_MSG_RESULT($USE_EXA)

        if test "x$USE_EXA" = xyes; then
                AC_DEFINE(USE_EXA, 1, [Build support for Exa])
        fi
fi

AC_CHECK_DECL(XSERVER_LIBPCIACCESS,
	      [XSERVER_LIBPCIACCESS=yes],[XSERVER_LIBPCIACCESS=no],
	      [#include "xorg-server.h"])
CPPFLAGS="$SAVE_CPPFLAGS"

AM_CONDITIONAL(USE_EXA, test "x$USE_EXA" = xyes)

if test "x$XSERVER_LIBPCIACCESS" = xyes; then
    PKG_CHECK_MODULES([PCIACCESS], [pciaccess >= 0.8.0])
    XORG_CFLAGS="$XORG_CFLAGS $PCIACCESS_CFLAGS"
fi
AM_CONDITIONAL(XSERVER_LIBPCIACCESS, test "x$XSERVER_LIBPCIACCESS" = xyes)

AC_CHECK_HEADERS([dev/wscons/wsconsio.h])

# Checks for headers/macros for byte swapping
# Known variants:
#	<byteswap.h> bswap_16, bswap_32, bswap_64  (glibc)
#	<sys/endian.h> __swap16, __swap32, __swap64 (OpenBSD)
#	<sys/endian.h> bswap16, bswap32, bswap64 (other BSD's)
#	and a fallback to local macros if none of the above are found

# if <byteswap.h> is found, assume it's the correct version
AC_CHECK_HEADERS([byteswap.h])

# if <sys/endian.h> is found, have to check which version
AC_CHECK_HEADER([sys/endian.h], [HAVE_SYS_ENDIAN_H="yes"], [HAVE_SYS_ENDIAN_H="no"])

if test "x$HAVE_SYS_ENDIAN_H" = "xyes" ; then
	AC_MSG_CHECKING([for __swap16 variant of <sys/endian.h> byteswapping macros])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <sys/types.h>
#include <sys/endian.h>
 ], [
int a = 1, b;
b = __swap16(a);
 ])
], [SYS_ENDIAN__SWAP='yes'], [SYS_ENDIAN__SWAP='no'])
	AC_MSG_RESULT([$SYS_ENDIAN__SWAP])

	AC_MSG_CHECKING([for bswap16 variant of <sys/endian.h> byteswapping macros])
	AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <sys/types.h>
#include <sys/endian.h>
 ], [
int a = 1, b;
b = bswap16(a);
 ])
], [SYS_ENDIAN_BSWAP='yes'], [SYS_ENDIAN_BSWAP='no'])
	AC_MSG_RESULT([$SYS_ENDIAN_BSWAP])

	if test "$SYS_ENDIAN_BSWAP" = "yes" ; then
		USE_SYS_ENDIAN_H=yes
		BSWAP=bswap
	else
		if test "$SYS_ENDIAN__SWAP" = "yes" ; then
			USE_SYS_ENDIAN_H=yes
			BSWAP=__swap
		else
			USE_SYS_ENDIAN_H=no
		fi
	fi

	if test "$USE_SYS_ENDIAN_H" = "yes" ; then
	    AC_DEFINE([USE_SYS_ENDIAN_H], 1,
		[Define to use byteswap macros from <sys/endian.h>])
	    AC_DEFINE_UNQUOTED([bswap_16], ${BSWAP}16,
			[Define to 16-bit byteswap macro])
	    AC_DEFINE_UNQUOTED([bswap_32], ${BSWAP}32,
			[Define to 32-bit byteswap macro])
	    AC_DEFINE_UNQUOTED([bswap_64], ${BSWAP}64,
			[Define to 64-bit byteswap macro])
	fi
fi

AC_SUBST([moduledir])

DRIVER_NAME=r128
AC_SUBST([DRIVER_NAME])

AC_MSG_NOTICE(
[Please change the Driver line in xorg.conf from "ati" to "r128" or install]
[the ati wrapper as well:]
[    https://gitlab.freedesktop.org/xorg/driver/xf86-video-ati]
)

AC_CONFIG_FILES([
                Makefile
                src/Makefile
                man/Makefile
])
AC_OUTPUT
