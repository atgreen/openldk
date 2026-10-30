#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Compare serialization, loader selection and reflection with JDK 25."""
import argparse
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
JDK = Path(os.environ['JAVA_HOME'])
BUILD = ROOT / '.cache/serialization-test'
BUILD.mkdir(parents=True, exist_ok=True)
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--runtime', type=Path, default=ROOT / 'openldk')
runtime = parser.parse_args().runtime.resolve()
wide_source = BUILD / 'WideClassProbe.java'
wide_source.write_text("""
class WideBase { public int inherited = 5; }
public class WideClassProbe extends WideBase {
    public static int state = 7;
    public static final String TEXT = "wide";
    public WideClassProbe() {}
    public int inheritedValue() { return inherited; }
    FIELDS
    static class Child extends WideClassProbe { int extra = 9; }
    public static void main(String[] args) throws Exception {
        WideClassProbe first = new WideClassProbe();
        first.f0 = 3; first.f8200 = 8;
        state = 13;
        WideClassProbe second = new WideClassProbe();
        Child child = new Child();
        if (first.f0 != 3 || first.f8200 != 8 || second.f0 != 0 || second.f8200 != 0
            || second.inherited != 5 || child.extra != 9 || child.inherited != 5 || state != 13
            || WideClassProbe.class.getField("TEXT").get(null) != TEXT)
            throw new AssertionError("wide field initialization");
        var references = new java.lang.ref.WeakReference<?>[1];
        Thread worker = new Thread(() -> {
            try { references[0] = loadCustom(args[0]); }
            catch (Exception e) { throw new RuntimeException(e); }
        });
        worker.start(); worker.join();
        if (references[0] == null) throw new AssertionError("custom loader worker");
        for (int i = 0; i < 100 && references[0].get() != null; i++) {
            System.gc(); Thread.sleep(20);
        }
        if (references[0].get() != null) throw new AssertionError("wide loader retained");
        System.out.println("wide class passed");
    }
    static java.lang.ref.WeakReference<ClassLoader> loadCustom(String directory) throws Exception {
        try (var loader = new java.net.URLClassLoader(
                new java.net.URL[] {java.nio.file.Path.of(directory).toUri().toURL()}, null)) {
            Class<?> type = Class.forName("WideClassProbe", true, loader);
            Object value = type.getConstructor().newInstance();
            if (value.getClass().getClassLoader() != loader
                || !type.getField("f8200").get(value).equals(0L)
                || !type.getMethod("inheritedValue").invoke(value).equals(5))
                throw new AssertionError("wide custom loader");
            return new java.lang.ref.WeakReference<>(loader);
        }
    }
}
""".replace('FIELDS', '\n'.join(f'public long f{i};' for i in range(8201))))
sources = [ROOT / 'test/serialization/SerializationProbe.java',
           ROOT / 'test/serialization/SerializationLoaderProbe.java', wide_source,
           ROOT / 'testsuite/gcj/PR26858.java']
sources += [ROOT / 'testsuite/gcj' / (name + '.java')
            for name in ('Serialization', 'pr21785', 'stringconst2', 'invokethrow')]
subprocess.run([str(JDK / 'bin/javac'), '--release', '25', '-d', str(BUILD)]
               + list(map(str, sources)), check=True, timeout=60)
for name in ('SerializationProbe', 'Serialization', 'pr21785', 'stringconst2', 'invokethrow', 'PR26858', 'WideClassProbe', 'SerializationLoaderProbe'):
    expected = None
    for engine in (JDK / 'bin/java', runtime):
        result = subprocess.run([str(engine), '-cp', str(BUILD), name, str(BUILD)],
                                cwd=BUILD, capture_output=True, text=True, timeout=120)
        (BUILD / f'{name}-{engine.name}.log').write_text(result.stdout + result.stderr)
        if result.returncode or result.stderr or (expected is not None and result.stdout != expected):
            raise RuntimeError(f'{engine.name} {name}:\n{result.stdout}{result.stderr}\nExpected: {expected!r}')
        expected = result.stdout
        print(f'PASS {engine.name} {name}', flush=True)
