#!/usr/bin/env python3
# Copyright (C) 2026 Anthony Green <green@moxielogic.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Recheck expected failures in isolated directories against OpenLDK and JDK 25.

Results and raw logs are resumable under --output. Jikes cases are all audited:
its historical harness reused Test.class even for sources declaring other mains.
Use one X display per worker for independent graphical tests, or omit --displays
for headless operation. No expectation files are changed by this command.
"""
import argparse
import concurrent.futures
import hashlib
import json
import os
from pathlib import Path
import re
import signal
import shutil
import subprocess
import threading
import time

ROOT = Path(__file__).resolve().parents[1]
SUITE = ROOT / 'testsuite'
LOCAL = threading.local()
LOCK = threading.Lock()
NEXT_WORKER = 0


def normalize(text):
    return '\n'.join(line.rstrip() for line in text.replace('\r', '').strip().splitlines())


def run(command, cwd, log, timeout, env):
    started = time.monotonic()
    with log.open('w') as stream:
        process = subprocess.Popen(command, cwd=cwd, env=env, stdout=stream,
                                   stderr=subprocess.STDOUT, start_new_session=True)
        try:
            status = process.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            process.wait()
            status = 'timeout'
    return {'status': status, 'seconds': round(time.monotonic() - started, 3),
            'log': str(log.relative_to(ROOT) if log.is_relative_to(ROOT) else log)}


def text(result):
    return (ROOT / result['log']).read_text(errors='replace')


def initialize_worker(displays):
    global NEXT_WORKER
    with LOCK:
        index = NEXT_WORKER
        NEXT_WORKER += 1
    LOCAL.env = dict(os.environ)
    if displays:
        LOCAL.env['DISPLAY'] = displays[index]


def find_main(output, preferred):
    mains = []
    current = None
    for line in output.splitlines():
        match = re.match(r'^(?:(?:public|abstract|final)\s+)*(?:class|interface|enum)\s+([^\s<{]+)', line)
        if match:
            current = match[1]
        if re.search(r'public static (?:final |synchronized |strictfp )*void main\(java\.lang\.String(?:\[\]|\.\.\.)\)', line):
            mains.append(current)
    return next((name for name in [preferred, 'Test'] if name in mains),
                mains[0] if len(mains) == 1 else None)


def uncaught_exception(output):
    """Primary exception only; Lisp backtraces and nested causes are excluded."""
    match = re.search(r'(?:Exception in thread "[^"\n]+" |Unhandled Java exception:\s*\n)([\w.$]+)', output)
    return (match[1], normalize(output[:match.start()])) if match else (None, None)


def compare_legacy(reference, actual, golden, jikes=False):
    reference_text, actual_text = normalize(text(reference)), normalize(text(actual))
    if not jikes and actual['status'] == 0 and (golden is None or actual_text == golden):
        return 'stale-pass'
    if reference['status'] == 'timeout':
        return 'reference-timeout'
    expected_exception = re.search(r'(?m)^(java\.[\w.$]+(?:Exception|Error))(?::|$)', golden or '')
    ref_exception, ref_prefix = uncaught_exception(text(reference))
    actual_exception, actual_prefix = uncaught_exception(text(actual))
    if jikes and expected_exception and ref_exception == expected_exception[1]:
        # These fixtures assert an uncaught exception, not the formatting of
        # 1990s VM diagnostics, optional helpful-NPE text, or source filenames.
        return ('stale-golden' if actual['status'] != 0 and actual['status'] != 'timeout'
                and actual_exception == ref_exception and actual_prefix == ref_prefix
                else 'runtime-failure')
    if actual_text == reference_text and actual['status'] == reference['status']:
        return 'stale-pass' if actual_text == golden else 'stale-golden'
    if reference['status'] == 0 or reference_text == golden or (jikes and not ref_exception):
        return 'runtime-failure'
    return 'reference-failure'


def classify_mauve(reference, actual):
    if reference['status'] == 'timeout':
        return 'reference-timeout'
    if reference['status'] != 0:
        return 'reference-failure'
    return 'stale-pass' if actual['status'] == 0 else 'runtime-failure'


def classify_obsolete_source(result, source):
    if result.get('compile', {}).get('status') == 0:
        content = source.read_text(errors='replace')
        if 'Integer.parseInt(parts[1])' in content and 'System.getProperty("java.version")' in content:
            result['classification'] = 'obsolete-version-parser'
            result['reason'] = ('Reads the second java.version component as the feature version; '
                                'crashes on 25 and misreads 25.0.x as Java 0. Needs porting before it can judge the VM.')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk',
                        help='OpenLDK executable to compare (default: repository image)')
    parser.add_argument('--manifest', type=Path, default=SUITE / 'expected-failures.txt')
    parser.add_argument('--summary', type=Path, default=SUITE / 'openldk.sum')
    parser.add_argument('--output', type=Path, default=ROOT / '.cache/failure-audit')
    parser.add_argument('--suite', choices=['gcj', 'jikestst', 'mauve', 'all'], default='all')
    parser.add_argument('--jobs', type=int, default=4)
    parser.add_argument('--timeout', type=int, default=65)
    parser.add_argument('--displays', default='')
    parser.add_argument('--fresh', action='store_true')
    parser.add_argument('--case', help='audit one source or Jikes expectation path')
    parser.add_argument('--inventory', type=Path, help='include all test IDs from a prior audit TSV')
    args = parser.parse_args()
    displays = args.displays.split(',') if args.displays else []
    if args.jobs < 1 or (displays and len(displays) < args.jobs):
        parser.error('provide a separate display for each worker')
    runtime = args.runtime.resolve()
    if not runtime.is_file():
        parser.error('--runtime must name an existing executable')
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    jdk = Path(os.environ['JAVA_HOME'])
    entries = set(re.findall(r'^(?:FAIL|XFAIL): (\S+) execution test$',
                             args.manifest.read_text(), re.M))
    if args.inventory:
        entries.update(line.split('\t')[0] for line in args.inventory.read_text().splitlines()
                       if line and not line.startswith('#') and not line.startswith('test\t'))
    entries.update(str(path.relative_to(SUITE)) for directory in ['derek', 'hpj']
                   for path in (SUITE / 'jikestst' / directory / 'ok/exec').glob('*.e'))
    summary = args.summary.read_text() if args.summary.exists() else ''
    entries.update(re.findall(r'^UNRESOLVED: (gcj/\S+\.java)', summary, re.M))
    entries = sorted(entry for entry in entries
                     if args.suite == 'all' or entry.startswith(args.suite + '/'))
    if args.case:
        if args.case not in entries:
            parser.error('--case must name a selected test')
        entries = [args.case]
    # Compile a common harness. A relative temp directory keeps each engine's
    # filesystem fixtures isolated while retaining the real source resources.
    common = output / 'common'
    common.mkdir(exist_ok=True)
    config = common / 'config.java'
    template = (SUITE / 'mauve/gnu/testlet/config.java.in').read_text()
    config.write_text(template.replace('@SRCDIR@', str(SUITE / 'mauve'))
                      .replace('@BUILDDIR@', str(ROOT)).replace('@TMPDIR@', '.'))
    if any(entry.startswith('mauve/') for entry in entries):
        harness = run([str(jdk / 'bin/javac'), '--release', '25', '-d', str(common),
                       '-sourcepath', str(SUITE / 'mauve'), str(config),
                       str(SUITE / 'mauve/openldkHarness.java')], output,
                      output / 'harness.log', args.timeout, os.environ)
        if harness['status'] != 0:
            raise SystemExit('Harness compilation failed: ' + harness['log'])
    metadata = {'java_home': str(jdk), 'runtime_sha256': hashlib.sha256(runtime.read_bytes()).hexdigest(),
                'manifest_sha256': hashlib.sha256(args.manifest.read_bytes()).hexdigest(),
                'timeout': args.timeout, 'displays': displays}
    (output / 'environment.json').write_text(json.dumps(metadata, indent=2) + '\n')

    def audit(entry):
        work = output / entry.replace('/', '__')
        work.mkdir(exist_ok=True)
        result_file = work / 'result.json'
        source = SUITE / entry
        if entry.startswith('jikestst/'):
            source = source.parent.parent.parent / 'src' / (source.stem + '.java')
        source_hash = hashlib.sha256(source.read_bytes()).hexdigest() if source.exists() else None
        cache_key = [metadata, source_hash]
        if result_file.exists() and not args.fresh:
            result = json.loads(result_file.read_text())
            stored = result.get('cache_key', [None, None])
            comparable = lambda metadata: {k: v for k, v in (metadata or {}).items() if k != 'manifest_sha256'}
            if stored[1] == source_hash and comparable(stored[0]) == comparable(metadata):
                if entry.startswith('mauve/') and 'jdk' in result:
                    result['classification'] = classify_mauve(result['jdk'], result['openldk'])
                classify_obsolete_source(result, source)
                result_file.write_text(json.dumps(result, indent=2) + '\n')
                return result
        result = {'test': entry, 'cache_key': cache_key}
        if not source.exists():
            result['classification'] = 'missing-source'
        else:
            classes = work / 'classes'
            if classes.exists():
                shutil.rmtree(classes)
            classes.mkdir()
            # Keep the original basename: some tests declare a public class.
            if entry.startswith('jikestst/'):
                public = re.search(r'public\s+(?:(?:final|abstract)\s+)*class\s+(\w+)', source.read_text())
                copied = work / (public[1] + '.java' if public else source.name)
                copied.write_bytes(source.read_bytes())
                source = copied
            sourcepath = str(SUITE / ('mauve' if entry.startswith('mauve/') else 'jikestst/hpj/src'))
            result['compile'] = run([str(jdk / 'bin/javac'), '--release', '25', '-d', str(classes),
                                     '-cp', str(common), '-sourcepath', sourcepath, str(source)], work,
                                    work / 'compile.log', args.timeout, LOCAL.env)
            if result['compile']['status'] != 0:
                result['classification'] = 'compile-rejected'
            else:
                cp = os.pathsep.join([str(classes), str(common)])
                if entry.startswith('mauve/'):
                    # Resource lookups need the source tree, after compiled classes.
                    cp += os.pathsep + str(SUITE / 'mauve')
                    arguments = ['mauve.openldkHarness', entry[len('mauve/'): -len('.java')].replace('/', '.')]
                else:
                    candidates = [str(p.relative_to(classes).with_suffix('')).replace('/', '.')
                                  for p in classes.rglob('*.class') if '$' not in p.name]
                    javap = run([str(jdk / 'bin/javap'), '-public', '-cp', cp] + candidates,
                                work, work / 'javap.log', args.timeout, LOCAL.env)
                    name = find_main(text(javap), source.stem)
                    arguments = [name] + (['1', '2', '3', '4', '5'] if entry.startswith('gcj/') else [])
                if not arguments[0]:
                    result['classification'] = 'non-executable-fixture'
                else:
                    for engine, executable in [('jdk', jdk / 'bin/java'), ('openldk', runtime)]:
                        cwd = work / engine
                        if cwd.exists():
                            shutil.rmtree(cwd)
                        cwd.mkdir()
                        result[engine] = run([str(executable), '-cp', cp] + arguments, cwd,
                                             work / (engine + '.log'), args.timeout, LOCAL.env)
                    reference, actual = result['jdk'], result['openldk']
                    if entry.startswith('mauve/'):
                        result['classification'] = classify_mauve(reference, actual)
                    else:
                        golden_path = SUITE / entry
                        if entry.startswith('gcj/'):
                            golden_path = golden_path.with_suffix('.out')
                        golden = normalize(golden_path.read_text()) if golden_path.exists() else None
                        result['classification'] = compare_legacy(reference, actual, golden, entry.startswith('jikestst/'))
                    result['main'] = arguments[0]
        classify_obsolete_source(result, source)
        temporary = result_file.with_suffix('.tmp')
        temporary.write_text(json.dumps(result, indent=2) + '\n')
        temporary.replace(result_file)
        return result

    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs, initializer=initialize_worker,
                                               initargs=(displays,)) as pool:
        futures = {pool.submit(audit, entry): entry for entry in entries}
        for future in concurrent.futures.as_completed(futures):
            result = future.result()
            results.append(result)
            print(f'{len(results)}/{len(entries)} {result["classification"]}: {result["test"]}', flush=True)
    (output / 'results.json').write_text(json.dumps(sorted(results, key=lambda x: x['test']), indent=2) + '\n')
    counts = {category: sum(r['classification'] == category for r in results)
              for category in sorted({r['classification'] for r in results})}
    print(json.dumps(counts, sort_keys=True), flush=True)


if __name__ == '__main__':
    main()
