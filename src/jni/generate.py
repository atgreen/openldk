#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
"""Generate ABI-exact forwarding functions from the installed JDK's jni.h."""
import pathlib
import re
import sys

header = pathlib.Path(sys.argv[1]).read_text()
body = header.split('struct JNINativeInterface_ {', 1)[1].split('\n};', 1)[0]
entries = re.findall(r'([\w\s*]+?)\(JNICALL\s*\*(\w+)\)\s*\((.*?)\);', body, re.S)
functions = []
names = []
for result, name, parameters in entries:
    result = ' '.join(result.split())
    parts = [' '.join(p.split()) for p in parameters.split(',')]
    args = []
    for part in parts:
        if part == '...':
            args.append(('...', ''))
        else:
            match = re.fullmatch(r'(.*?)(\w+)', part)
            args.append((match[1].strip(), match[2]))
    declaration = ', '.join(t + (' ' + n if n else '') for t, n in args)
    code = [f'static {result} JNICALL bridge_{name}({declaration}) {{']
    is_variadic = args[-1][0] in ('...', 'va_list')
    if is_variadic:
        # All variadic JNI APIs have an A counterpart taking jvalue[].
        base = name[:-1] if name.endswith('V') else name
        fixed = args[:-1]
        member = next(n for t, n in fixed if t == 'jmethodID')
        if args[-1][0] == '...':
            code.append(f'  va_list ap; va_start(ap, {fixed[-1][1]});')
        else:
            code.append(f'  va_list ap; va_copy(ap, {args[-1][1]});')
        code.append(f'  jvalue values[256] = {{0}}; pack_va({member}, ap, values); va_end(ap);')
        call = f'bridge_{base}A(' + ', '.join(n for _, n in fixed) + ', values)'
        code.append(f'  {call};' if result == 'void' else f'  return {call};')
    else:
        code.append(f'  jvalue a[{len(args)}] = {{0}}, r = {{0}};')
        for index, (typ, arg) in enumerate(args):
            if typ in ('jfloat', 'jdouble'):
                code.append(f'  a[{index}].{ "f" if typ == "jfloat" else "d"} = {arg};')
            else:
                code.append(f'  a[{index}].j = (int64_t)(intptr_t){arg};')
        code.append(f'  r.j = dispatch("{name}", a);')
        if result == 'void':
            code.append('  (void)r;')
        elif result in ('jfloat', 'jdouble'):
            code.append(f'  return r.{"f" if result == "jfloat" else "d"};')
        else:
            code.append(f'  return ({result})(intptr_t)r.j;')
    code.append('}')
    functions.append((name, result, declaration, '\n'.join(code)))
    names.append(name)

out = ['/* Generated from jni.h by src/jni/generate.py. */']
out += [f'static {r} JNICALL bridge_{n}({d});' for n, r, d, _ in functions]
out += [c for _, _, _, c in functions]
out += ['static const struct JNINativeInterface_ native_table = {']
out += [f'  .{name} = bridge_{name},' for name in names]
out += ['};']
pathlib.Path(sys.argv[2]).write_text('\n'.join(out) + '\n')
