/*
 * Copyright 2000 through 2004 by Marc Aurele La France (TSI @ UQV), tsi@xfree86.org
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting documentation, and
 * that the name of Marc Aurele La France not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Marc Aurele La France makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as-is" without express or implied warranty.
 *
 * MARC AURELE LA FRANCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL MARC AURELE LA FRANCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#ifndef _R128_VERSION_H_
#define _R128_VERSION_H_ 1

#undef  R128_NAME
#undef  R128_DRIVER_NAME
#undef  R128_VERSION_MAJOR
#undef  R128_VERSION_MINOR
#undef  R128_VERSION_PATCH
#undef  R128_VERSION_SUFFIX
#undef  R128_VERSION_CURRENT
#undef  R128_DRI_VERSION_MAJOR
#undef  R128_DRI_VERSION_MINOR
#undef  R128_DRI_VERSION_PATCH

#define R128_NAME          "R128"
#define R128_DRIVER_NAME   "r128"

#define R128_VERSION_MAJOR  PACKAGE_VERSION_MAJOR
#define R128_VERSION_MINOR  PACKAGE_VERSION_MINOR
#define R128_VERSION_PATCH  PACKAGE_VERSION_PATCHLEVEL
#define R128_VERSION_SUFFIX PACKAGE_VERSION_SUFFIX

#define R128_VERSION_CURRENT \
    ((R128_VERSION_MAJOR << 20) | \
     (R128_VERSION_MINOR << 10) | \
     (R128_VERSION_PATCH))

/* DDX interface version reported to the r128 DRI client */
#define R128_DRI_VERSION_MAJOR 4
#define R128_DRI_VERSION_MINOR 1
#define R128_DRI_VERSION_PATCH 0

#endif /* _R128_VERSION_H_ */
