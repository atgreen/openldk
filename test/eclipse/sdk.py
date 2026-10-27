#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Install the pinned, unmodified Eclipse SDK into a user-owned directory."""
import hashlib
from pathlib import Path
import platform
import shutil
import sys
import tarfile
import tempfile
import urllib.request

ROOT = Path(__file__).resolve().parents[2]
CACHE = ROOT / '.cache/eclipse'
DEFAULT_INSTALL = CACHE / 'eclipse'
URL = ('https://archive.eclipse.org/eclipse/downloads/drops4/R-4.41-202608281142/'
       'eclipse-SDK-4.41-linux-gtk-x86_64.tar.gz')
SHA512 = ('503f757a096256f719139db67e2537478542585844876e0d68f702ca51dbc1a0282d3'
          'a5fa5023d77c1013c1b35a90525cc02aeed1af17ced26bde293da46d2cf')


def valid_install(path):
    return ((path / 'configuration/config.ini').is_file()
            and any((path / 'plugins').glob('org.eclipse.equinox.launcher_*.jar')))


def ensure_sdk(install=DEFAULT_INSTALL, download=False):
    install = Path(install).expanduser().resolve()
    if valid_install(install):
        return install
    if install.exists():
        raise RuntimeError(f'Not an Eclipse installation: {install}; choose a nonexistent installation path')
    if not download:
        raise RuntimeError(f'Eclipse not installed at {install}; use --download or --install-dir PATH')
    if sys.platform != 'linux' or platform.machine() not in ('x86_64', 'AMD64'):
        raise RuntimeError('The pinned Eclipse download requires Linux x86_64')
    CACHE.mkdir(parents=True, exist_ok=True)
    archive = CACHE / 'eclipse-sdk.tar.gz'
    if not archive.exists():
        print(f'Downloading Eclipse SDK 4.41 (276 MiB): {URL}', flush=True)
        with tempfile.NamedTemporaryFile(dir=CACHE, suffix='.part', delete=False) as target:
            temporary = Path(target.name)
            try:
                with urllib.request.urlopen(URL, timeout=60) as source:
                    shutil.copyfileobj(source, target)
                target.close()
                temporary.replace(archive)
            finally:
                temporary.unlink(missing_ok=True)
    with archive.open('rb') as stream:
        if hashlib.file_digest(stream, 'sha512').hexdigest() != SHA512:
            raise RuntimeError(f'Eclipse SDK checksum mismatch; remove {archive} and retry')
    install.parent.mkdir(parents=True, exist_ok=True)
    print(f'Installing Eclipse SDK in {install}', flush=True)
    with tempfile.TemporaryDirectory(prefix='.eclipse-install-', dir=install.parent) as staging:
        with tarfile.open(archive) as source:
            source.extractall(staging, filter='data')
        unpacked = Path(staging) / 'eclipse'
        if not valid_install(unpacked):
            raise RuntimeError('Eclipse archive did not contain a complete installation')
        unpacked.rename(install)
    return install
