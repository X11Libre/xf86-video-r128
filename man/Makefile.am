#
# Copyright (c) 2005, Oracle and/or its affiliates.
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice (including the next
# paragraph) shall be included in all copies or substantial portions of the
# Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

drivermandir = $(DRIVER_MAN_DIR)

driverman_PRE = r128.man

driverman_DATA = $(driverman_PRE:man=@DRIVER_MAN_SUFFIX@)

EXTRA_DIST = r128.man

CLEANFILES = $(driverman_DATA)


# String replacements in MAN_SUBSTS now come from xorg-macros.m4 via configure


SUFFIXES = .$(DRIVER_MAN_SUFFIX) .man

.man.$(DRIVER_MAN_SUFFIX):
	$(AM_V_GEN)$(SED) $(MAN_SUBSTS) < $< > $@
