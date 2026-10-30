#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Verify real X11 pixels, window events, and disposal on an isolated display."""
import argparse
import ctypes as C
import os
from pathlib import Path
import queue
import re
import subprocess
import sys
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/awt-test'
JDK = Path(os.environ['JAVA_HOME'])
TITLE = b'OpenLDK X11 window probe'


class XImage(C.Structure):
    _fields_ = [(name, C.c_int) for name in ('width', 'height', 'xoffset', 'format')] + [
        ('data', C.c_void_p)] + [(name, C.c_int) for name in (
            'byte_order', 'bitmap_unit', 'bitmap_bit_order', 'bitmap_pad', 'depth',
            'bytes_per_line', 'bits_per_pixel')] + [(name, C.c_ulong) for name in (
                'red_mask', 'green_mask', 'blue_mask')]


class Display:
    def __init__(self, name):
        self.lib = C.CDLL('libX11.so.6')
        signatures = {
            'XOpenDisplay': (C.c_void_p, [C.c_char_p]),
            'XCloseDisplay': (C.c_int, [C.c_void_p]),
            'XDefaultRootWindow': (C.c_ulong, [C.c_void_p]),
            'XQueryTree': (C.c_int, [C.c_void_p, C.c_ulong, C.POINTER(C.c_ulong),
                                    C.POINTER(C.c_ulong), C.POINTER(C.POINTER(C.c_ulong)), C.POINTER(C.c_uint)]),
            'XFetchName': (C.c_int, [C.c_void_p, C.c_ulong, C.POINTER(C.c_void_p)]),
            'XFree': (C.c_int, [C.c_void_p]),
            'XGetImage': (C.POINTER(XImage), [C.c_void_p, C.c_ulong, C.c_int, C.c_int,
                                            C.c_uint, C.c_uint, C.c_ulong, C.c_int]),
            'XGetPixel': (C.c_ulong, [C.POINTER(XImage), C.c_int, C.c_int]),
            'XDestroyImage': (C.c_int, [C.POINTER(XImage)]),
        }
        for function, (result, arguments) in signatures.items():
            call = getattr(self.lib, function)
            call.restype, call.argtypes = result, arguments
        self.handle = self.lib.XOpenDisplay(name.encode())
        if not self.handle:
            raise RuntimeError(f'Cannot open X display {name}')
        self.root = self.lib.XDefaultRootWindow(self.handle)

    def close(self):
        self.lib.XCloseDisplay(self.handle)

    def children(self, parent):
        root, owner, count = C.c_ulong(), C.c_ulong(), C.c_uint()
        children = C.POINTER(C.c_ulong)()
        if not self.lib.XQueryTree(self.handle, parent, C.byref(root), C.byref(owner),
                                  C.byref(children), C.byref(count)):
            raise RuntimeError(f'Cannot query X window {parent}')
        try:
            return [children[i] for i in range(count.value)]
        finally:
            if children:
                self.lib.XFree(children)

    def find_window(self, parent=None):
        for window in self.children(self.root if parent is None else parent):
            name = C.c_void_p()
            self.lib.XFetchName(self.handle, window, C.byref(name))
            try:
                if name and C.string_at(name) == TITLE:
                    return window
            finally:
                if name:
                    self.lib.XFree(name)
            found = self.find_window(window)
            if found is not None:
                return found
        return None

    def check_pixels(self, window):
        image = self.lib.XGetImage(self.handle, window, 0, 0, 200, 160, C.c_ulong(-1), 2)
        if not image:
            raise AssertionError('No server-side window image')
        try:
            masks = [getattr(image.contents, component + '_mask') for component in ('red', 'green', 'blue')]
            if not all(masks):
                raise AssertionError('Pixel probe needs a TrueColor X display')
            for x, y, expected in [(15, 15, 0x123456), (40, 40, 0xe05020),
                                   (90, 80, 0x2060e0), (155, 120, 0x899aab),
                                   (145, 55, 0xe05020), (155, 30, 0x20c040)]:
                pixel = self.lib.XGetPixel(image, x, y)
                actual = []
                for mask in masks:
                    shift = (mask & -mask).bit_length() - 1
                    actual.append(round(((pixel & mask) >> shift) * 255 / (mask >> shift)))
                wanted = [(expected >> shift) & 255 for shift in (16, 8, 0)]
                if any(abs(a - b) > 8 for a, b in zip(actual, wanted)):
                    raise AssertionError(f'X server pixel ({x},{y}): {actual}, expected {wanted}')
        finally:
            self.lib.XDestroyImage(image)


def wait_for_marker(lines, marker, timeout):
    deadline = time.monotonic() + timeout
    while True:
        try:
            line = lines.get(timeout=max(.01, deadline - time.monotonic()))
        except queue.Empty:
            raise AssertionError(f'Timed out waiting for {marker}') from None
        if line is None:
            raise AssertionError(f'Runtime exited before {marker}')
        if any(diagnostic in line for diagnostic in (
                'Exception during peer initialization', 'Exception during disposal',
                'UnsatisfiedLinkError', 'CORRUPTION WARNING', 'terminated with error')):
            raise AssertionError('Runtime failed during window initialization: ' + line.strip())
        if line.strip() == marker:
            return


def check(engine, display):
    log, lines = [], queue.Queue()
    log_path = BUILD / (engine.name + '-x11-window.log')
    log_path.write_text('')
    process = subprocess.Popen([str(engine), '-cp', str(BUILD), 'X11WindowProbe'], cwd=ROOT,
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               text=True, env=dict(os.environ, DISPLAY=display))

    def collect():
        with log_path.open('w') as stream:
            for line in process.stdout:
                stream.write(line)
                stream.flush()
                log.append(line)
                lines.put(line)
        lines.put(None)

    reader = threading.Thread(target=collect, daemon=True)
    reader.start()
    connection = Display(display)
    try:
        wait_for_marker(lines, 'X11_WINDOW_READY', 150)
        window = connection.find_window()
        if window is None:
            raise AssertionError('No actual X window with the requested title')
        connection.check_pixels(window)
        process.stdin.write('close\n')
        process.stdin.flush()
        wait_for_marker(lines, 'X11_WINDOW_CLOSED', 30)
        if process.poll() is not None:
            raise AssertionError('Runtime exited before checking native window disposal')
        if connection.find_window() is not None:
            raise AssertionError('Native window survived disposal')
        process.stdin.write('exit\n')
        process.stdin.flush()
        process.wait(timeout=30)
        reader.join(timeout=5)
        output = ''.join(log)
        if process.returncode or 'X11 window rendering, events, and disposal passed' not in output:
            raise AssertionError('Window shutdown or event delivery failed')
        for diagnostic in ('Exception during peer initialization', 'Exception during disposal',
                           'UnsatisfiedLinkError', 'CORRUPTION WARNING', 'terminated with error'):
            if diagnostic in output:
                raise AssertionError(f'Runtime reported {diagnostic}')
        print(f'{engine.name}: real X11 pixels, window events, and disposal passed', flush=True)
    finally:
        if process.poll() is None:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)
        reader.join(timeout=5)
        connection.close()
        log_path.write_text(''.join(log))


def check_robot(engine, display):
    result = subprocess.run([str(engine), '-cp', str(BUILD), 'X11RobotProbe'], cwd=ROOT,
                            capture_output=True, text=True, timeout=150,
                            env=dict(os.environ, DISPLAY=display))
    output = result.stdout + result.stderr
    (BUILD / (engine.name + '-x11-robot.log')).write_text(output)
    diagnostics = ('Exception during peer initialization', 'Exception during disposal',
                   'UnsatisfiedLinkError', 'CORRUPTION WARNING', 'terminated with error')
    if (result.returncode or 'X11 Robot input passed' not in result.stdout
            or any(diagnostic in output for diagnostic in diagnostics)):
        raise AssertionError('Robot input failed:\n' + output)
    print(f'{engine.name}: real Robot keyboard and mouse input passed', flush=True)


def check_selection(engine, display):
    result = subprocess.run([str(engine), '-cp', str(BUILD), 'X11SelectionProbe'], cwd=ROOT,
                            capture_output=True, text=True, timeout=150,
                            env=dict(os.environ, DISPLAY=display))
    output = result.stdout + result.stderr
    (BUILD / (engine.name + '-x11-selection.log')).write_text(output)
    diagnostics = ('Exception in thread', 'UnsatisfiedLinkError',
                   'CORRUPTION WARNING', 'terminated with error')
    if (result.returncode or 'X11 primary selection round trip passed' not in result.stdout
            or any(diagnostic in output for diagnostic in diagnostics)):
        raise AssertionError('X11 primary selection failed:\n' + output)
    print(f'{engine.name}: X11 primary selection ownership and contents passed', flush=True)


def visual_command(engine, arguments):
    options = ['-Dsun.java2d.xrender=false', '-Dawt.robot.gtk=false']
    options.append('--add-exports=java.desktop/sun.awt=ALL-UNNAMED')
    return [str(engine), *options, '-cp', str(BUILD), 'X11VisualProbe', *arguments]


def check_visuals(engine, display):
    # Select the JDK's software X11 path too. Its default XRender pipeline
    # leaves an 8-bit Xvfb window unpainted even after the paint callback.
    result = subprocess.run(visual_command(engine, ['screen']), cwd=ROOT,
                            capture_output=True, text=True, timeout=240,
                            env=dict(os.environ, DISPLAY=display))
    output = result.stdout + result.stderr
    (BUILD / (engine.name + '-x11-visuals.log')).write_text(output)
    markers = ('Exception during peer initialization', 'Exception during disposal',
               'UnsatisfiedLinkError', 'CORRUPTION WARNING', 'terminated with error')
    if (result.returncode or 'X11 visual models, compatible images, and palette pixels passed' not in result.stdout
            or any(marker in output for marker in markers)):
        raise AssertionError('X11 visual models failed:\n' + output)
    counts = next(line for line in result.stdout.splitlines() if line.startswith('X11_CONFIGURATIONS '))
    print(f'{engine.name}: {counts}; visual images and server pixels passed', flush=True)
    return counts


def check_all_visuals(engine, display):
    """Return models and painted samples keyed by physical visual, not array order."""
    def probe(arguments, label):
        result = subprocess.run(visual_command(engine, arguments), cwd=ROOT,
                                capture_output=True, text=True, timeout=240,
                                env=dict(os.environ, DISPLAY=display))
        output = result.stdout + result.stderr
        (BUILD / (engine.name + '-x11-' + label + '.log')).write_text(output)
        if (result.returncode or 'X11 visual models, compatible images, and palette pixels passed' not in result.stdout
                or any(marker in output for marker in ('Exception during peer initialization',
                       'Exception during disposal', 'UnsatisfiedLinkError', 'CORRUPTION WARNING',
                       'terminated with error'))):
            raise AssertionError(f'{engine.name} {label}:\n{output}')
        return result.stdout

    configurations = re.findall(r'^X11_VISUAL (\d+) .*vis=0x([0-9a-fA-F]+)\] (\w+)$',
                                probe(['list'], 'all-models'), re.MULTILINE)
    if not configurations:
        raise AssertionError('No physical X11 visual descriptions')
    samples = {}
    for index, visual, model in configurations:
        pixels = tuple(int(rgb) for _, rgb in re.findall(r'^(?:PIXEL|PATTERN) (\d+) (\d+)$',
                       probe(['screen', 'config:' + index, 'patterns'], 'visual-' + visual), re.MULTILINE))
        if len(pixels) != 10:
            raise AssertionError(f'Incomplete visual {visual} samples: {pixels}')
        key = int(visual, 16)
        if key in samples:
            raise AssertionError(f'Duplicate physical visual {visual}')
        # Dynamic indexed visuals can allocate different legal palettes.
        # Compare the shared primary colors on every visual, and the extra
        # physical-gray rendering cases where the full ramp is fixed. Keep
        # all samples in the logs for investigating quantization differences.
        samples[key] = (model, pixels[:5], pixels[5:] if model == 'ComponentColorModel' else ())
        print(f'{engine.name}: visual 0x{visual} {model}: {pixels}', flush=True)
    return samples


def check_resize(engine, display):
    command = [str(engine), '--add-exports=java.desktop/sun.awt=ALL-UNNAMED',
               '-Dsun.java2d.xrender=false', '-Dawt.robot.gtk=false',
               '-cp', str(BUILD), 'X11ResizeProbe']
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=240,
                            env=dict(os.environ, DISPLAY=display))
    output = result.stdout + result.stderr
    (BUILD / (engine.name + '-x11-resize.log')).write_text(output)
    if (result.returncode or 'X11 backing rasters follow grow and shrink operations passed' not in result.stdout
            or any(marker in output for marker in ('Exception during peer initialization',
                   'Exception during disposal', 'UnsatisfiedLinkError', 'CORRUPTION WARNING',
                   'terminated with error'))):
        raise AssertionError(f'X11 resize failed:\n{output}')
    print(f'{engine.name}: growing and shrinking display-backed rasters passed', flush=True)


def check_native(display):
    command = ['sbcl', '--dynamic-space-size', '8192', '--non-interactive',
               '--eval', '(require :asdf)',
               '--eval', '(progn (push (uiop:getcwd) asdf:*central-registry*) (asdf:load-system :openldk))',
               '--eval', '(openldk:initialize)', '--load', str(ROOT / 'test/awt/x11-native.lisp')]
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=300,
                            env=dict(os.environ, DISPLAY=display, LDK_X11_PROBE_DIR=str(BUILD)))
    output = result.stdout + result.stderr
    (BUILD / 'sbcl-x11-native.log').write_text(output)
    markers = ['X11 save preflight preserves live displays and threads',
               'X11 callback errors return safely through C']
    if result.returncode or not all(marker in result.stdout for marker in markers):
        raise AssertionError('ASDF X11 native lifecycle checks failed:\n' + output)
    print('ASDF: X11 image preflight and native callback recovery passed', flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--display', default=os.environ.get('DISPLAY'))
    parser.add_argument('--reference-only', action='store_true')
    parser.add_argument('--visuals-only', action='store_true',
                        help='check visual models and pixels, including on an 8-bit X server')
    parser.add_argument('--all-visuals', action='store_true',
                        help='paint and capture each physical visual; may be slow on large servers')
    options = parser.parse_args()
    if not options.display:
        parser.error('--display or DISPLAY must identify an isolated X server (such as Xvfb)')
    BUILD.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD),
                    str(ROOT / 'test/awt/X11WindowProbe.java'),
                    str(ROOT / 'test/awt/X11Probe.java'),
                    str(ROOT / 'test/awt/X11SelectionProbe.java'),
                    str(ROOT / 'test/awt/X11RobotProbe.java'),
                    str(ROOT / 'test/awt/X11VisualProbe.java'),
                    str(ROOT / 'test/awt/X11ResizeProbe.java')], check=True, timeout=60)
    if options.all_visuals:
        expected = check_all_visuals(JDK / 'bin/java', options.display)
        if not options.reference_only:
            actual = check_all_visuals(options.runtime.resolve(), options.display)
            if actual != expected:
                differences = {hex(visual): (expected.get(visual), actual.get(visual))
                               for visual in expected.keys() | actual.keys()
                               if expected.get(visual) != actual.get(visual)}
                raise AssertionError(f'Physical visual models or pixels differ: {differences}')
        return
    if options.visuals_only:
        expected = check_visuals(JDK / 'bin/java', options.display)
        if not options.reference_only and check_visuals(options.runtime.resolve(), options.display) != expected:
            raise AssertionError('Advertised visual configuration counts differ from the JDK')
        return
    check(JDK / 'bin/java', options.display)
    check_robot(JDK / 'bin/java', options.display)
    check_resize(JDK / 'bin/java', options.display)
    check_selection(JDK / 'bin/java', options.display)
    visual_counts = check_visuals(JDK / 'bin/java', options.display)
    if not options.reference_only:
        check(options.runtime.resolve(), options.display)
        check_robot(options.runtime.resolve(), options.display)
        check_resize(options.runtime.resolve(), options.display)
        check_selection(options.runtime.resolve(), options.display)
        if check_visuals(options.runtime.resolve(), options.display) != visual_counts:
            raise AssertionError('Advertised visual configuration counts differ from the JDK')
        check_native(options.display)
        subprocess.run([sys.executable, str(ROOT / 'test/awt/check-capture.py'),
                        '--runtime', str(options.runtime.resolve()), '--display', options.display],
                       cwd=ROOT, check=True, timeout=1050)


if __name__ == '__main__':
    main()
