#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Build ordinary JNI libraries and run identical probes on HotSpot/OpenLDK."""
import argparse
import hashlib
import os
from pathlib import Path
import shlex
import subprocess
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/jni/test'
SOURCE = ROOT / 'test/jni'


def run(command, marker=None):
    result = subprocess.run([str(part) for part in command], cwd=ROOT,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            text=True, timeout=180)
    if result.returncode or (marker and marker not in result.stdout):
        raise RuntimeError(f'{shlex.join(map(str, command))}\n{result.stdout}')
    if marker:
        print(f'{Path(command[0]).name}: {marker}', flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--zstd', action='store_true', help='also download and test pinned zstd-jni')
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk', help='OpenLDK executable to test')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    includes = [f'-I{JDK}/include', f'-I{JDK}/include/linux']
    for source, compiler, name in [('probe.c', os.environ.get('CC', 'cc'), 'jniprobe'),
                                    ('probe.cpp', os.environ.get('CXX', 'c++'), 'jniprobe_cpp'),
                                    ('bad.c', os.environ.get('CC', 'cc'), 'jniprobe_bad')]:
        run(shlex.split(compiler) + ['-Wall', '-Wextra', '-Werror', '-fPIC', '-shared'] + includes
            + [SOURCE / source, '-pthread', '-o', BUILD / f'lib{name}.so'])
    for value in [1, 2]:
        run(shlex.split(os.environ.get('CC', 'cc')) + ['-Wall', '-Wextra', '-Werror', '-fPIC', '-shared']
            + includes + [f'-DTWIN_VALUE={value}', SOURCE / 'twin.c', '-o', BUILD / f'libjniprobe_twin{value}.so'])
    run([JDK / 'bin/javac', '-d', BUILD, SOURCE / 'JNIProbe.java', SOURCE / 'LibraryOwner.java', SOURCE / 'JNIDefined.java', SOURCE / 'NativeTwin.java', SOURCE / 'JNIForeignShutdown.java'])
    engines = [[JDK / 'bin/java', '--enable-native-access=ALL-UNNAMED'], [options.runtime.resolve()]]
    for engine in engines:
        run(engine + [f'-Djava.library.path={BUILD}', '-cp', BUILD, 'JNIProbe', BUILD / 'libjniprobe.so', BUILD],
            'JNI probe passed')
        run(engine + [f'-Djava.library.path={BUILD}', '-cp', BUILD, 'JNIProbe', BUILD / 'libjniprobe.so', BUILD, 'runtime'],
            'JNI probe passed')
        run(engine + ['-cp', BUILD, 'JNIForeignShutdown', BUILD / 'libjniprobe.so'], 'JNI worker finished')
    if options.zstd:
        jar = BUILD / 'zstd-jni-1.5.7-6.jar'
        if not jar.exists():
            urllib.request.urlretrieve('https://repo.maven.apache.org/maven2/com/github/luben/zstd-jni/1.5.7-6/' + jar.name, jar)
        if hashlib.sha256(jar.read_bytes()).hexdigest() != '8d6feb1da335f3ab13c584c613e23c7b3c61b392e37956872057baf8f0ca1d6f':
            raise RuntimeError('zstd-jni checksum mismatch')
        run([JDK / 'bin/javac', '-cp', jar, '-d', BUILD, SOURCE / 'ZstdProbe.java'])
        for engine in engines:
            run(engine + ['-cp', f'{BUILD}{os.pathsep}{jar}', 'ZstdProbe'], 'Third-party zstd JNI passed')


if __name__ == '__main__':
    main()
