#!/usr/bin/env python3
"""Generate raw classfiles and compare OpenLDK verification with HotSpot 25.
No ASM dependency: malformed bytecodes and stack maps are emitted directly.
"""
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import os
from pathlib import Path
import struct
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
JAVA = Path(os.environ['JAVA_HOME']) / 'bin/java'


def u2(value):
    return struct.pack('>H', value)


def u4(value):
    return struct.pack('>I', value)


def utf(value):
    encoded = value.encode('utf-8')
    return b'\x01' + u2(len(encoded)) + encoded


def classfile(name, code, descriptor='()I', stack=4, locals_=4, maps=None,
              constructor=None, handlers=(), version=69, extra=(), trailing=b'', magic=0xcafebabe,
              class_flags=0x21, method_flags=9, duplicate_method=False, field_flags=None, duplicate_field=False):
    """A public class with Object's constructor, an empty main, and a test method."""
    # Pool indices are intentionally stable so bad instructions can name holes.
    pool = [utf(name), b'\x07' + u2(1), utf('java/lang/Object'), b'\x07' + u2(3),
            utf('<init>'), utf('()V'), b'\x0c' + u2(5) + u2(6),
            b'\x0a' + u2(4) + u2(7), utf('Code'), utf('main'),
            utf('([Ljava/lang/String;)V'), utf('test'), utf(descriptor), utf('StackMapTable')] + list(extra)

    def method(flags, name_index, descriptor_index, body, maximum, local_count, frames=None, exceptions=()):
        nested = b'' if frames is None else u2(14) + u4(len(frames)) + frames
        attr = (u2(maximum) + u2(local_count) + u4(len(body)) + body + u2(len(exceptions))
                + b''.join(struct.pack('>HHHH', *entry) for entry in exceptions)
                + u2(frames is not None) + nested)
        return (u2(flags) + u2(name_index) + u2(descriptor_index) + u2(1)
                + u2(9) + u4(len(attr)) + attr)

    methods = [method(1, 5, 6, constructor if constructor is not None else bytes([42, 183, 0, 8, 177]), 2, 1),
               method(9, 10, 11, bytes([177]), 0, 1),
               method(method_flags, 12, 13, bytes(code), stack, locals_, maps, handlers)]
    if field_flags is not None:
        pool.append(utf('I'))
    if duplicate_method:
        methods.append(methods[-1])
    fields = [] if field_flags is None else [u2(field_flags) + u2(12) + u2(len(pool)) + u2(0)]
    if duplicate_field:
        fields *= 2
    return (u4(magic) + u2(0) + u2(version) + u2(len(pool) + 1)
            + b''.join(pool) + u2(class_flags) + u2(2) + u2(4) + u2(0)
            + u2(len(fields)) + b''.join(fields)
            + u2(len(methods)) + b''.join(methods) + u2(0) + trailing)


JOIN = [26, 153, 0, 7, 4, 167, 0, 4, 5, 172]
STRING_LENGTH = [utf('java/lang/String'), b'\x07' + u2(15), utf('length'), utf('()I'),
                 b'\x0c' + u2(17) + u2(18), b'\x0a' + u2(16) + u2(19)]
LIST_SIZE = [utf('java/util/List'), b'\x07' + u2(15), utf('size'), utf('()I'),
             b'\x0c' + u2(17) + u2(18), b'\x0b' + u2(16) + u2(19)]
CASES = [
    ('ValidInt', True, dict(code=[3, 172], stack=1, locals_=0)),
    ('ValidLongDup', True, dict(code=[9, 92, 97, 173], descriptor='()J', stack=4)),
    ('ValidArray', True, dict(code=[42, 190, 172], descriptor='([I)I', locals_=1)),
    ('ValidNullReturn', True, dict(code=[1, 176], descriptor='()Ljava/lang/Object;')),
    ('ValidNew', True, dict(code=[187, 0, 4, 89, 183, 0, 8, 176], descriptor='()Ljava/lang/Object;')),
    ('ValidJoin', True, dict(code=JOIN, descriptor='(I)I', maps=bytes([0, 2, 8, 64, 1]))),
    ('ValidBackEdge', True, dict(code=[167, 0, 0], maps=bytes([0, 1, 0]))),
    ('ValidWide', True, dict(code=[3, 196, 54, 1, 0, 196, 21, 1, 0, 172], locals_=257)),
    ('ValidStringReceiver', True, dict(code=[42, 182, 0, 20, 172], descriptor='(Ljava/lang/String;)I', extra=STRING_LENGTH)),
    ('ValidInterfaceReceiver', True, dict(code=[42, 185, 0, 20, 1, 0, 172], descriptor='(Ljava/util/List;)I', extra=LIST_SIZE)),
    ('ValidInterfaceObject', True, dict(code=[42, 185, 0, 20, 1, 0, 172], descriptor='(Ljava/lang/Object;)I', extra=LIST_SIZE)),
    ('ValidCatchSupertype', True, dict(code=[3, 172, 87, 3, 172], handlers=[(0, 2, 2, 0)], maps=bytes([0, 1, 66, 7, 0, 4]))),
    ('BadFinalAbstractClass', False, dict(code=[3, 172], class_flags=0x431)),
    ('BadDuplicateMethod', False, dict(code=[3, 172], duplicate_method=True)),
    ('BadMethodVisibility', False, dict(code=[3, 172], method_flags=0x0b)),
    ('BadAbstractStaticMethod', False, dict(code=[3, 172], method_flags=0x409)),
    ('BadFinalVolatileField', False, dict(code=[3, 172], field_flags=0x51)),
    ('BadDuplicateField', False, dict(code=[3, 172], field_flags=1, duplicate_field=True)),
    ('BadReceiverClass', False, dict(code=[42, 182, 0, 20, 172], descriptor='(Ljava/lang/Object;)I', extra=STRING_LENGTH)),
    ('BadReceiverPrimitive', False, dict(code=[26, 182, 0, 20, 172], descriptor='(I)I', extra=STRING_LENGTH)),
    ('BadInterfaceCount', False, dict(code=[42, 185, 0, 20, 0, 0, 172], descriptor='(Ljava/util/List;)I', extra=LIST_SIZE)),
    ('BadInterfaceReserved', False, dict(code=[42, 185, 0, 20, 1, 1, 172], descriptor='(Ljava/util/List;)I', extra=LIST_SIZE)),
    ('BadInterfaceArray', False, dict(code=[42, 185, 0, 20, 1, 0, 172], descriptor='([I)I', extra=LIST_SIZE)),
    ('BadCatchType', False, dict(code=[3, 172, 87, 3, 172], handlers=[(0, 2, 2, 4)], maps=bytes([0, 1, 66, 7, 0, 4]))),
    ('BadCatchStack', False, dict(code=[3, 172, 87, 3, 172], handlers=[(0, 2, 2, 0)], maps=bytes([0, 1, 66, 1]))),
    ('BadBackEdgeMap', False, dict(code=[167, 0, 0])),
    ('BadWideSecondSlot', False, dict(code=[9, 63, 27, 172], locals_=2)),
    ('BadOverwrittenWide', False, dict(code=[9, 63, 3, 60, 30, 173], descriptor='()J', locals_=2)),
    ('BadMagic', False, dict(code=[3, 172], magic=0)),
    ('BadTrailing', False, dict(code=[3, 172], trailing=b'garbage')),
    ('BadEmptyCode', False, dict(code=[])),
    ('BadDescriptor', False, dict(code=[3, 172], descriptor='()Q')),
    ('BadUnderflow', False, dict(code=[87, 3, 172])),
    ('BadReturnNull', False, dict(code=[1, 172])),
    ('BadLocal', False, dict(code=[26, 172], locals_=0)),
    ('BadBranch', False, dict(code=[167, 0, 2, 3, 172])),
    ('BadConstant', False, dict(code=[18, 0, 172])),
    ('BadMaxStack', False, dict(code=[3, 4, 96, 172], stack=1)),
    ('BadDupLong', False, dict(code=[9, 89, 173], descriptor='()J')),
    ('BadArrayLoad', False, dict(code=[42, 3, 46, 172], descriptor='([J)I')),
    ('BadMissingMaps', False, dict(code=JOIN, descriptor='(I)I')),
    ('BadFrameType', False, dict(code=JOIN, descriptor='(I)I', maps=bytes([0, 2, 8, 64, 2]))),
    ('BadFrameOffset', False, dict(code=JOIN, descriptor='(I)I', maps=bytes([0, 1, 6]))),
    ('BadFrameTruncated', False, dict(code=JOIN, descriptor='(I)I', maps=bytes([0, 1, 247]))),
    ('BadFrameTag', False, dict(code=JOIN, descriptor='(I)I', maps=bytes([0, 1, 200]))),
    ('BadFrameTrailing', False, dict(code=JOIN, descriptor='(I)I', maps=bytes([0, 0, 0]))),
    ('BadConstructor', False, dict(code=[3, 172], constructor=bytes([177]))),
    ('BadDoubleInit', False, dict(code=[3, 172], constructor=bytes([42, 183, 0, 8, 42, 183, 0, 8, 177]))),
    ('BadUninitializedReturn', False, dict(code=[187, 0, 4, 176], descriptor='()Ljava/lang/Object;')),
    ('BadNewConstructorOwner', False, dict(code=[187, 0, 2, 89, 183, 0, 8, 176], descriptor='()Ljava/lang/Object;')),
    ('BadTruncated', False, dict(code=[16])),
    ('BadWide', False, dict(code=[196, 0, 0, 0, 172])),
    ('BadNewArray', False, dict(code=[3, 188, 99, 176], descriptor='()[I')),
    ('BadDeadCode', False, dict(code=[3, 172, 0])),
    ('BadJsr', False, dict(code=[168, 0, 0])),
    ('BadSwitchRange', False, dict(code=[3, 170, 0, 0] + [0] * 8 + [255] * 4)),
    ('BadExceptionRange', False, dict(code=[3, 172], handlers=[(1, 0, 0, 4)])),
]


def run(command):
    result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            text=True, timeout=90)
    return result.returncode, result.stdout


def main():
    with tempfile.TemporaryDirectory(prefix='openldk-verifier-') as directory:
        for name, valid, options in CASES:
            (Path(directory) / (name + '.class')).write_bytes(classfile(name, **options))
            hotspot = run([str(JAVA), '-Xverify:all', '-cp', directory, name])
            openldk = run([str(ROOT / 'openldk'), '-cp', directory, name])
            for engine, (status, output) in [('HotSpot', hotspot), ('OpenLDK', openldk)]:
                if (status == 0) != valid:
                    raise AssertionError(f'{name}: {engine} unexpected status {status}\n{output}')
                if not valid and not any(error in output for error in ('VerifyError', 'ClassFormatError')):
                    raise AssertionError(f'{name}: {engine} failed without a verification exception\n{output}')
            print(f'PASS {name}', flush=True)
        compiler = JAVA.parent / 'javac'
        status, output = run([str(compiler), '-d', directory, str(Path(__file__).with_name('CatchVerification.java'))])
        if status:
            raise AssertionError(f'Cannot compile Java exception test\n{output}')
        for name, exception in [('BadMagic', 'java.lang.ClassFormatError'), ('BadReturnNull', 'java.lang.VerifyError')]:
            for engine in [str(JAVA), str(ROOT / 'openldk')]:
                status, output = run([engine, '-cp', directory, 'CatchVerification',
                                      str(Path(directory) / (name + '.class')), exception])
                if status or f'Caught {exception}' not in output:
                    raise AssertionError(f'{engine}: guest could not catch {exception}\n{output}')
            print(f'PASS guest catch {exception}', flush=True)
        hierarchy = Path(__file__).with_name('hierarchy')
        status, output = run([str(compiler), '-d', directory] + [str(p) for p in hierarchy.glob('*.java')])
        if status:
            raise AssertionError(f'Cannot compile hierarchy test\n{output}')
        for engine in [str(JAVA), str(ROOT / 'openldk')]:
            status, output = run([engine, '-cp', directory, 'HierarchyLoaderProbe', directory])
            if status or 'Defining-loader verification hierarchy passed' not in output:
                raise AssertionError(f'{engine}: defining-loader hierarchy failed\n{output}')
        print('PASS defining-loader hierarchy', flush=True)
        for engine in [str(JAVA), str(ROOT / 'openldk')]:
            status, output = run([engine, '-cp', directory, 'ArrayLoaderProbe', directory])
            if status or 'Array hierarchy passed' not in output:
                raise AssertionError(f'{engine}: delegated array hierarchy failed\n{output}')
        print('PASS delegated array hierarchy', flush=True)
    print(f'{len(CASES)} verifier differential cases passed')


if __name__ == '__main__':
    main()
