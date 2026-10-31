#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare Lucene vector searches in mapped and in-memory indexes against JDK 25."""
import argparse
import hashlib
import os
from pathlib import Path
import subprocess
import tempfile
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/lucene-test'
VERSION = '10.6.0'
SHA256 = '211424f423ff1afeb891095c2142a20a572e545906251f28b8faaa408d01f934'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--jar', type=Path, help='use an existing Lucene core 10.6.0 jar')
    args = parser.parse_args()
    jdk = Path(os.environ['JAVA_HOME'])
    BUILD.mkdir(parents=True, exist_ok=True)
    jar = args.jar.resolve() if args.jar else BUILD / f'lucene-core-{VERSION}.jar'
    if not jar.exists():
        if args.jar:
            raise SystemExit(f'Jar does not exist: {jar}')
        url = f'https://repo.maven.apache.org/maven2/org/apache/lucene/lucene-core/{VERSION}/{jar.name}'
        with urllib.request.urlopen(url, timeout=60) as response:
            data = response.read()
        if hashlib.sha256(data).hexdigest() != SHA256:
            raise SystemExit('Downloaded Lucene jar checksum mismatch')
        jar.write_bytes(data)
    if hashlib.sha256(jar.read_bytes()).hexdigest() != SHA256:
        raise SystemExit('Lucene jar checksum mismatch')
    subprocess.run([str(jdk / 'bin/javac'), '-cp', str(jar), '-d', str(BUILD),
                    str(ROOT / 'test/lucene/LuceneVectorProbe.java')], check=True, timeout=60)
    reference = None
    for engine in (jdk / 'bin/java', args.runtime.resolve()):
        with tempfile.TemporaryDirectory(prefix='index-', dir=BUILD) as directory:
            result = subprocess.run([str(engine), '--enable-native-access=ALL-UNNAMED',
                                     '-cp', os.pathsep.join((str(jar), str(BUILD))),
                                     'LuceneVectorProbe', directory],
                                    capture_output=True, text=True, timeout=600)
        (BUILD / f'{engine.name}.log').write_text(result.stdout + result.stderr)
        if result.returncode or 'Lucene vector searches passed' not in result.stdout:
            raise SystemExit(result.stdout + result.stderr)
        if reference is None:
            reference = result.stdout
        elif result.stdout != reference:
            raise SystemExit(f'Lucene results differ from JDK 25:\n{result.stdout}')
        print(f'{engine.name}: Lucene vector searches PASS', flush=True)


if __name__ == '__main__':
    main()
