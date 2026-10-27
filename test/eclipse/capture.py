# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import ctypes as c
import sys
gdk=c.CDLL('libgdk-3.so.0'); pix=c.CDLL('libgdk_pixbuf-2.0.so.0')
gdk.gdk_init_check.argtypes=[c.c_void_p,c.c_void_p]; gdk.gdk_init_check.restype=c.c_int
gdk.gdk_get_default_root_window.restype=c.c_void_p
gdk.gdk_pixbuf_get_from_window.argtypes=[c.c_void_p,c.c_int,c.c_int,c.c_int,c.c_int]; gdk.gdk_pixbuf_get_from_window.restype=c.c_void_p
pix.gdk_pixbuf_savev.argtypes=[c.c_void_p,c.c_char_p,c.c_char_p,c.c_void_p,c.c_void_p,c.c_void_p]; pix.gdk_pixbuf_savev.restype=c.c_int
assert gdk.gdk_init_check(None,None)
image=gdk.gdk_pixbuf_get_from_window(gdk.gdk_get_default_root_window(),0,0,1024,768)
assert image
assert pix.gdk_pixbuf_savev(image,sys.argv[1].encode(),b'png',None,None,None)
