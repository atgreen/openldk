#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Check Java lock identity, OS byte ranges, contention, sharing, and release."""
import argparse
import errno
import fcntl
import os
from pathlib import Path
import select
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/nio-test'
JDK = Path(os.environ['JAVA_HOME'])


def line(process, expected):
    if not select.select([process.stdout], [], [], 180)[0]:
        raise RuntimeError(f'Timeout awaiting {expected}')
    actual = process.stdout.readline().strip()
    if actual != expected:
        raise RuntimeError(f'Expected {expected}, got {actual}; see {BUILD / "stderr.log"}')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(JDK / 'bin/javac'), '-d', str(BUILD), str(ROOT / 'test/nio/FileLockProbe.java'),
                    str(ROOT / 'test/nio/FileInputProbe.java'), str(ROOT / 'test/nio/BufferScalarProbe.java')],
                   check=True, timeout=60)
    path = BUILD / 'locked.bin'
    path.write_bytes(bytes(64))
    for engine in [JDK / 'bin/java', options.runtime.resolve()]:
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'BufferScalarProbe'],
                                cwd=ROOT, capture_output=True, text=True, timeout=120)
        if result.returncode or 'ByteBuffer unaligned scalar access' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        result = subprocess.run([str(engine), '-cp', str(BUILD), 'FileInputProbe', str(BUILD / 'bulk-read.bin')],
                                cwd=ROOT, capture_output=True, text=True, timeout=180)
        if result.returncode or 'FileInputStream bulk reads' not in result.stdout:
            raise RuntimeError(f'{engine}:\n{result.stdout}\n{result.stderr}')
        print(f'{engine.name}: {result.stdout.strip()}', flush=True)
        command = [str(engine), '-cp', str(BUILD), 'FileLockProbe', str(path)]
        for mode in ['exclusive', 'shared']:
            with (BUILD / 'stderr.log').open('w') as errors, path.open('r+b') as file:
                process = subprocess.Popen(command + [mode], cwd=ROOT, stdin=subprocess.PIPE,
                                           stdout=subprocess.PIPE, stderr=errors, text=True)
                try:
                    line(process, 'LOCKED')
                    try:
                        fcntl.lockf(file, fcntl.LOCK_EX | fcntl.LOCK_NB, 16, 0)
                    except OSError as error:
                        if error.errno not in [errno.EACCES, errno.EAGAIN]:
                            raise
                    else:
                        raise AssertionError('Java lock did not exclude another process')
                    fcntl.lockf(file, fcntl.LOCK_EX | fcntl.LOCK_NB, 16, 16)
                    fcntl.lockf(file, fcntl.LOCK_UN, 16, 16)
                    if mode == 'shared':
                        fcntl.lockf(file, fcntl.LOCK_SH | fcntl.LOCK_NB, 16, 0)
                        fcntl.lockf(file, fcntl.LOCK_UN, 16, 0)
                    process.stdin.write('\n'); process.stdin.flush()
                    line(process, 'RELEASED')
                    fcntl.lockf(file, fcntl.LOCK_EX | fcntl.LOCK_NB, 16, 0)
                    fcntl.lockf(file, fcntl.LOCK_UN, 16, 0)
                    process.stdin.write('\n'); process.stdin.flush()
                    if process.wait(timeout=20):
                        raise RuntimeError('Java lock holder failed')
                finally:
                    if process.poll() is None:
                        process.kill(); process.wait()
            print(f'{engine.name}: {mode} identity, ranges, sharing, release passed', flush=True)
        with path.open('r+b') as file:
            fcntl.lockf(file, fcntl.LOCK_EX | fcntl.LOCK_NB, 16, 0)
            result = subprocess.run(command + ['blocked'], cwd=ROOT, capture_output=True,
                                    text=True, timeout=180)
            if result.returncode or result.stdout.strip() != 'CONTENDED':
                raise RuntimeError(f'Java contention check failed: {result.stdout}\n{result.stderr}')
        print(f'{engine.name}: contention result passed', flush=True)


if __name__ == '__main__':
    main()
