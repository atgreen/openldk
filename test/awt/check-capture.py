#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare Robot captures of foreign-client windows with private colormaps."""
import argparse
import ctypes as C
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])


class Color(C.Structure):
    _fields_ = [('pixel', C.c_ulong), ('red', C.c_ushort), ('green', C.c_ushort),
                ('blue', C.c_ushort), ('flags', C.c_ubyte), ('pad', C.c_ubyte)]


class Rectangle(C.Structure):
    _fields_ = [('x', C.c_short), ('y', C.c_short), ('width', C.c_ushort), ('height', C.c_ushort)]


class Fixture:
    def __init__(self, name):
        self.lib = C.CDLL('libX11.so.6')
        self.ext = C.CDLL('libXext.so.6')
        signatures = {
            'XOpenDisplay': (C.c_void_p, [C.c_char_p]),
            'XCloseDisplay': (C.c_int, [C.c_void_p]),
            'XDefaultRootWindow': (C.c_ulong, [C.c_void_p]),
            'XDefaultVisual': (C.c_void_p, [C.c_void_p, C.c_int]),
            'XCreateColormap': (C.c_ulong, [C.c_void_p, C.c_ulong, C.c_void_p, C.c_int]),
            'XAllocColor': (C.c_int, [C.c_void_p, C.c_ulong, C.POINTER(Color)]),
            'XCreateSimpleWindow': (C.c_ulong, [C.c_void_p, C.c_ulong, C.c_int, C.c_int,
                                               C.c_uint, C.c_uint, C.c_uint, C.c_ulong, C.c_ulong]),
            'XSetWindowColormap': (C.c_int, [C.c_void_p, C.c_ulong, C.c_ulong]),
            'XMapWindow': (C.c_int, [C.c_void_p, C.c_ulong]),
            'XClearWindow': (C.c_int, [C.c_void_p, C.c_ulong]),
            'XSync': (C.c_int, [C.c_void_p, C.c_int]),
        }
        for function, (result, arguments) in signatures.items():
            call = getattr(self.lib, function)
            call.restype, call.argtypes = result, arguments
        self.ext.XShapeCombineRectangles.restype = None
        self.ext.XShapeCombineRectangles.argtypes = [C.c_void_p, C.c_ulong, C.c_int, C.c_int, C.c_int,
                                                    C.POINTER(Rectangle), C.c_int, C.c_int, C.c_int]
        self.handle = self.lib.XOpenDisplay(name.encode())
        if not self.handle:
            raise RuntimeError(f'Cannot open {name}')
        self.root = self.lib.XDefaultRootWindow(self.handle)
        self.visual = self.lib.XDefaultVisual(self.handle, 0)

    def close(self):
        self.lib.XCloseDisplay(self.handle)

    def window(self, parent, x, y, width, height, border, background, edge):
        colormap = self.lib.XCreateColormap(self.handle, self.root, self.visual, 0)

        def color(rgb):
            value = Color(0, ((rgb >> 16) & 255) * 257, ((rgb >> 8) & 255) * 257,
                          (rgb & 255) * 257, 7, 0)
            if not self.lib.XAllocColor(self.handle, colormap, C.byref(value)):
                raise AssertionError('Cannot allocate fixture color')
            return value.pixel

        bg, bd = color(background), color(edge)
        window = self.lib.XCreateSimpleWindow(self.handle, parent, x, y, width, height, border, bd, bg)
        self.lib.XSetWindowColormap(self.handle, window, colormap)
        self.lib.XMapWindow(self.handle, window)
        self.lib.XClearWindow(self.handle, window)
        return window

    def paint(self, shaped):
        self.window(self.root, 170, 170, 160, 140, 0, 0x00ffff, 0)
        parent = self.window(self.root, 200, 200, 100, 80, 4, 0xff0000, 0x0000ff)
        self.window(parent, -10, 10, 60, 50, 2, 0x00ff00, 0xffff00)
        self.window(self.root, 230, 220, 50, 50, 2, 0xffffff, 0x0000ff)
        if shaped:
            region = (Rectangle * 2)(Rectangle(-4, -4, 108, 34), Rectangle(-4, 30, 34, 54))
            self.ext.XShapeCombineRectangles(self.handle, parent, 0, 0, 0, region, 2, 0, 0)
        self.lib.XSync(self.handle, 0)


def expected_pixels(shaped):
    """Rasterize the fixture geometry independently of either JVM's capture."""
    pixels = []
    for y in range(180, 300):
        for x in range(180, 320):
            rgb = 0x00ffff  # Lower window covers the complete capture rectangle.
            bounding = (200 <= x < 308 and 200 <= y < 288)
            if shaped:
                bounding = bounding and (y < 234 or x < 234)
            content = 204 <= x < 304 and 204 <= y < 284
            if bounding:
                rgb = 0xff0000 if content else 0x0000ff
                if content and 194 <= x < 258 and 214 <= y < 268:
                    rgb = 0x00ff00 if 196 <= x < 256 and 216 <= y < 266 else 0xffff00
            if 230 <= x < 284 and 220 <= y < 274:
                rgb = 0xffffff if 232 <= x < 282 and 222 <= y < 272 else 0x0000ff
            pixels.append(rgb)
    return pixels


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--display', required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='capture-test-', dir=ROOT / '.cache') as temporary:
        build = Path(temporary)
        subprocess.run([str(JDK / 'bin/javac'), '-d', str(build), str(ROOT / 'test/awt/X11CaptureProbe.java')],
                       check=True, timeout=60)
        for shaped in [False, True]:
            fixture = Fixture(args.display)
            try:
                fixture.paint(shaped)
                expected = expected_pixels(shaped)
                for name, engine in [('java', JDK / 'bin/java'), ('openldk', args.runtime.resolve())]:
                    result = subprocess.run([str(engine), '-Dsun.java2d.xrender=false', '-Dawt.robot.gtk=false',
                                             '-cp', str(build), 'X11CaptureProbe', '180', '180', '140', '120'],
                                            env=dict(os.environ, DISPLAY=args.display), cwd=ROOT,
                                            capture_output=True, text=True, timeout=240)
                    if result.returncode:
                        raise AssertionError(f'{name}:\n{result.stdout}\n{result.stderr}')
                    pixels = [int(value, 16) for line in result.stdout.splitlines() for value in line.split(',')]
                    assert len(pixels) == 140 * 120
                    assert len(set(pixels)) >= 5, 'Fixture failed to expose private colormap colors'
                    if pixels != expected:
                        differences = [i for i, (a, b) in enumerate(zip(pixels, expected)) if a != b]
                        # JDK 25 multiVis.c clips rectangular window bounds without
                        # consulting XShape, so it reads undefined obscured pixels
                        # through holes. Use the independently painted geometry
                        # for shaped windows rather than reproducing that defect.
                        if name == 'java' and shaped:
                            print(f'JDK shaped-window reference differs from geometry at '
                                  f'{len(differences)} pixels', flush=True)
                        else:
                            offset = differences[0]
                            raise AssertionError(f'{name}, shaped={shaped}: pixel '
                                                 f'({offset % 140}, {offset // 140}) '
                                                 f'got {pixels[offset]:06x}, expected {expected[offset]:06x}')
                print(f'Foreign colormaps, overlap, clipped children and borders: shaped={shaped} passed', flush=True)
            finally:
                fixture.close()


if __name__ == '__main__':
    main()
