#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Check stored ZIP metadata, streamed entries, and URLClassLoader CRC diagnostics."""
import argparse
import io
import os
from pathlib import Path
import struct
import subprocess
import zipfile

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/zip-test'
JDK = Path(os.environ['JAVA_HOME'])


class NonSeekable(io.BytesIO):
    def seekable(self):
        return False

    def seek(self, *args):
        raise io.UnsupportedOperation()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD)]
                   + [str(ROOT / 'test/zip' / name) for name in ['ZipMetadataProbe.java', 'ZipPayload.java', 'ZipCallback.java', 'ArrayLoaderProbe.java', 'ZipConcurrentProbe.java', 'BufferLoaderProbe.java']],
                   check=True, timeout=60)
    for streamed in [False, True]:
        output = NonSeekable() if streamed else io.BytesIO()
        with zipfile.ZipFile(output, 'w') as archive:
            archive.comment = b'central directory metadata regression'
            archive.writestr('stored.bin', bytes(range(256)), compress_type=zipfile.ZIP_STORED)
            archive.writestr('deflated.bin', bytes(range(256)) * 10, compress_type=zipfile.ZIP_DEFLATED)
            archive.writestr('unicode/\u03bb.txt', b'hello', compress_type=zipfile.ZIP_DEFLATED)
            archive.writestr('ZipMessages.properties', b'message=bundle resource\n', compress_type=zipfile.ZIP_DEFLATED)
            payload = bytearray((BUILD / 'ZipPayload.class').read_bytes())
            archive.writestr('ZipPayload.class', payload,
                             compress_type=zipfile.ZIP_DEFLATED)
            archive.writestr('ZipCallback.class', (BUILD / 'ZipCallback.class').read_bytes(),
                             compress_type=zipfile.ZIP_DEFLATED)
            for nested in BUILD.glob('ZipPayload$*.class'):
                archive.writestr(nested.name, nested.read_bytes(), compress_type=zipfile.ZIP_DEFLATED)
        data = output.getvalue()
        for corrupt in [False, True]:
            contents = bytearray(data)
            if corrupt:
                offset = contents.index(b'PK\x01\x02')
                while True:
                    length, extra, comment = struct.unpack_from('<HHH', contents, offset + 28)
                    if contents[offset + 46:offset + 46 + length] == b'ZipPayload.class':
                        contents[offset + 16] ^= 1
                        break
                    offset += 46 + length + extra + comment
            path = BUILD / f'{streamed}-{corrupt}.jar'
            path.write_bytes(contents)
            for engine in [JDK / 'bin/java', options.runtime.resolve()]:
                args = [str(engine), '-cp', str(BUILD), 'ZipMetadataProbe', str(path)]
                if corrupt:
                    args.append('corrupt')
                result = subprocess.run(args, cwd=ROOT, capture_output=True, text=True, timeout=180)
                if result.returncode or 'ZIP metadata and class loading passed:' not in result.stdout:
                    raise RuntimeError(f'{args}\n{result.stdout}\n{result.stderr}')
                print(f'{engine.name} streamed={streamed}: {result.stdout.strip()}', flush=True)
    for engine in [JDK / 'bin/java', options.runtime.resolve()]:
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'ArrayLoaderProbe',
                                 str(BUILD / 'False-False.jar')], cwd=ROOT,
                                capture_output=True, text=True, timeout=180)
        if result.returncode or 'Array loader identity' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'BufferLoaderProbe',
                                 str(BUILD / 'False-False.jar')], cwd=ROOT,
                                capture_output=True, text=True, timeout=180)
        if result.returncode or 'ByteBuffer class definitions passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'ZipConcurrentProbe',
                                 str(BUILD / 'False-False.jar')], cwd=ROOT,
                                capture_output=True, text=True, timeout=180)
        if result.returncode or 'Concurrent ZIP reads passed' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)


if __name__ == '__main__':
    main()
