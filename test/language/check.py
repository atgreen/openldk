#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare records, sealed types, and patterns across their original releases and 25."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
BUILD = ROOT / '.cache/language-test'
JDK = Path(os.environ['JAVA_HOME'])
PROBES = [
    ('RecordProbe', (16, 25), 'Records: primitive/reference semantics, floating edges, empty records, and reflective metadata passed'),
    ('SealedProbe', (17, 25), 'Sealed types: reflection, initialization, descendants, inheritance rejection, and loader identity passed'),
    ('PatternProbe', (21, 25), 'Patterns: record deconstruction, sealed exhaustiveness, guards, restart, nulls, enums, and accessor failures passed'),
    ('PrimitivePatternProbe', (25,), 'Primitive patterns: exact conversions, boxed switches, narrowing boundaries, and guards passed'),
]


def compile_sources(directory, sources, release):
    directory.mkdir(parents=True, exist_ok=True)
    inputs = []
    for name, content in sources.items():
        path = directory / (name + '.java')
        path.write_text(content + '\n')
        inputs.append(str(path))
    subprocess.run([str(JDK / 'bin/javac'), '--release', str(release), '-d', str(directory)] + inputs,
                   check=True, timeout=60)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
    parser.add_argument('--reference-only', action='store_true')
    options = parser.parse_args()
    engines = [JDK / 'bin/java']
    if not options.reference_only:
        engines.append(options.runtime.resolve())
    for probe, releases, marker in PROBES:
        for release in releases:
            directory = BUILD / f'{probe}-{release}'
            directory.mkdir(parents=True, exist_ok=True)
            flags = ['--enable-preview'] if probe == 'PrimitivePatternProbe' else []
            subprocess.run([str(JDK / 'bin/javac'), '--release', str(release)] + flags
                           + ['-d', str(directory), str(ROOT / 'test/language' / (probe + '.java'))],
                           check=True, timeout=60)
            arguments = []
            if probe == 'SealedProbe':
                legacy = directory / 'legacy'
                compile_sources(legacy, {
                    'SealRoot': 'public class SealRoot {}',
                    'SealContract': 'public interface SealContract {}',
                    'Rogue': 'public class Rogue extends SealRoot {}',
                    'RogueImpl': 'public class RogueImpl implements SealContract {}',
                }, release)
                compile_sources(directory, {
                    'SealRoot': 'public sealed class SealRoot permits Allowed {}',
                    'Allowed': 'public final class Allowed extends SealRoot {}',
                    'SealContract': 'public sealed interface SealContract permits AllowedImpl {}',
                    'AllowedImpl': 'public final class AllowedImpl implements SealContract {}',
                    'FilterRoot': 'public sealed class FilterRoot permits PresentChild, MissingAllowed, NotDirectChild {}',
                    'PresentChild': 'public final class PresentChild extends FilterRoot {}',
                    'MissingAllowed': 'public final class MissingAllowed extends FilterRoot {}',
                    'NotDirectChild': 'public final class NotDirectChild extends FilterRoot {}',
                }, release)
                (directory / 'MissingAllowed.class').unlink()
                compile_sources(directory, {'NotDirectChild': 'public class NotDirectChild {}'}, release)
                malformed = bytearray((directory / 'SealRoot.class').read_bytes())
                position, index = 10, 1
                count = int.from_bytes(malformed[8:10], 'big')
                sizes = {3: 4, 4: 4, 5: 8, 6: 8, 7: 2, 8: 2, 9: 4, 10: 4, 11: 4,
                         12: 4, 15: 3, 16: 2, 17: 4, 18: 4, 19: 2, 20: 2}
                while index < count:
                    tag = malformed[position]
                    position += 1
                    if tag == 1:
                        position += 2 + int.from_bytes(malformed[position:position + 2], 'big')
                    else:
                        position += sizes[tag]
                    index += 2 if tag in (5, 6) else 1
                access = int.from_bytes(malformed[position:position + 2], 'big') | 0x10
                malformed[position:position + 2] = access.to_bytes(2, 'big')
                invalid = directory / 'FinalSealed.class'
                invalid.write_bytes(malformed)
                arguments = [str(legacy / 'Rogue.class'), str(legacy / 'RogueImpl.class'),
                             str(directory / 'Allowed.class'), str(invalid)]
            for engine in engines:
                result = subprocess.run([str(engine)] + flags + ['-cp', str(directory), probe] + arguments,
                                        capture_output=True, text=True, timeout=120)
                (directory / f'{engine.name}.log').write_text(result.stdout + result.stderr)
                if result.returncode or marker not in result.stdout or 'terminated with error' in result.stderr:
                    raise AssertionError(f'{engine} {probe} --release {release}: exit={result.returncode}\n{result.stdout}\n{result.stderr}')
                print(f'{engine.name} --release {release}: {result.stdout.strip()}', flush=True)


if __name__ == '__main__':
    main()
