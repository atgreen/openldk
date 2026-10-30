#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Check isolated preferences persistence and native POSIX lock behavior."""
import argparse
import errno
import fcntl
import os
from pathlib import Path
import resource
import select
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])


def limit_descriptors():
    resource.setrlimit(resource.RLIMIT_NOFILE, (128, 128))


def restrictive_umask():
    os.umask(0o777)


def line(process, expected):
    if not select.select([process.stdout], [], [], 240)[0]:
        raise AssertionError(f'Timeout waiting for {expected}')
    actual = process.stdout.readline().strip()
    if actual != expected:
        raise AssertionError(f'Expected {expected}, got {actual}')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='preferences-', dir=ROOT / '.cache') as temporary:
        build = Path(temporary)
        subprocess.run([str(JDK / 'bin/javac'), '-d', str(build),
                        str(ROOT / 'test/prefs/PreferencesProbe.java'),
                        str(ROOT / 'test/prefs/PreferencesLockProbe.java')],
                       check=True, timeout=60)
        for index, engine in enumerate([JDK / 'bin/java', args.runtime.resolve()]):
            home = build / str(index)
            home.mkdir()
            options = [str(engine), f'-Duser.home={home}',
                       f'-Djava.util.prefs.userRoot={home}',
                       '-cp', str(build)]

            def run(probe, *arguments):
                result = subprocess.run(options + [probe, *map(str, arguments)],
                                        cwd=ROOT, capture_output=True, text=True,
                                        timeout=300, preexec_fn=limit_descriptors)
                if result.returncode or 'PASS ' not in result.stdout:
                    raise AssertionError(f'{engine}: {arguments}\n{result.stdout}\n{result.stderr}')

            for mode in ['write', 'read', 'removed']:
                run('PreferencesProbe', mode)
            print(f'{engine.name}: persistence, Unicode, XML import/export, listeners, removal passed', flush=True)
            missing = home / 'missing.lock'
            run('PreferencesLockProbe', 'missing', missing)
            assert not missing.exists()
            path = home / 'native.lock'
            for mode in ['exclusive', 'shared', 'exclusive']:
                if path.exists():
                    path.chmod(0o640)
                expected_mode = 0o640 if path.exists() else 0o600
                with (home / 'lock-errors.log').open('w') as errors:
                    process = subprocess.Popen(options + ['PreferencesLockProbe', mode, str(path)],
                                               cwd=ROOT, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                               stderr=errors, text=True, preexec_fn=restrictive_umask)
                    try:
                        line(process, 'LOCKED')
                        assert path.stat().st_mode & 0o777 == expected_mode
                        with path.open('r+b') as file:
                            try:
                                fcntl.lockf(file, fcntl.LOCK_EX | fcntl.LOCK_NB)
                            except OSError as error:
                                assert error.errno in (errno.EACCES, errno.EAGAIN)
                            else:
                                raise AssertionError('Native lock failed to exclude another process')
                            if mode == 'shared':
                                fcntl.lockf(file, fcntl.LOCK_SH | fcntl.LOCK_NB)
                                fcntl.lockf(file, fcntl.LOCK_UN)
                            process.stdin.write('\n')
                            process.stdin.flush()
                            line(process, 'RELEASED')
                            fcntl.lockf(file, fcntl.LOCK_EX | fcntl.LOCK_NB)
                            fcntl.lockf(file, fcntl.LOCK_UN)
                        process.stdin.write('\n')
                        process.stdin.flush()
                        assert process.wait(timeout=30) == 0
                    finally:
                        if process.poll() is None:
                            process.kill()
                            process.wait()
                        process.stdin.close()
                        process.stdout.close()
            path.chmod(0o600)
            run('PreferencesLockProbe', 'chmod', path)
            assert path.stat().st_mode & 0o777 == 0o640
            with path.open('r+b') as file:
                fcntl.lockf(file, fcntl.LOCK_EX | fcntl.LOCK_NB)
                run('PreferencesLockProbe', 'contended', path)
                fcntl.lockf(file, fcntl.LOCK_UN)
            print(f'{engine.name}: shared/exclusive locks, permissions, errno, contention cleanup passed',
                  flush=True)


if __name__ == '__main__':
    main()
