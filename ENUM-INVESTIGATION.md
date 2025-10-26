# OpenLDK: Enum/Universe Debug Notes (javac AIOOBE) - RESOLVED

## Goal

- Track down and fix the javac failure under OpenLDK where an
  ArrayIndexOutOfBoundsException occurs while iterating the enum
  `com.sun.tools.javac.main.Option` via EnumSet/RegularEnumSet.
- Confirm whether the failure is due to math/bit-iteration, or an
  undersized enum "universe" array returned by reflection/values().

## RESOLUTION - Bug Fixed (Oct 26, 2025)

**ROOT CAUSE**: Bug in LUSHR (unsigned long right shift) instruction codegen

**Location**: `src/codegen.lisp:1038` in `ir-lushr` method

**The Bug**: Copy-paste error where `value2` (shift amount) was used twice:
```lisp
; WRONG - shifts the shift amount instead of the value!
(ash (logand ,(code (codegen (value2 insn) context)) #xFFFFFFFFFFFFFFFF)
```

**The Fix**: Use `value1` (the value to shift) instead:
```lisp
; CORRECT - shifts the actual value
(ash (logand ,(code (codegen (value1 insn) context)) #xFFFFFFFFFFFFFFFF)
```

**Impact**: This broke RegularEnumSet's bit mask creation. When EnumSet.allOf()
tried to create a mask for 61 elements using `~0L >>> 3`, it was actually
shifting 3 instead of ~0L, resulting in bit 63 being incorrectly set in the
bit vector. This caused the iterator to try accessing index 63 in a 61-element array.

**Verification**: After fix, javac no longer throws ArrayIndexOutOfBoundsException
when iterating Option enum constants.

## Hypothesis

- EnumSet’s bit iteration is correct (trailingZeros can legitimately
  return 63). The universe array provided by reflection/values() is
  under-populated (length 61 instead of 62), so accessing ordinal 63
  fails. Root cause is likely in enum initialization or static field
  emission for the `Option` enum (i.e., not all constants make it into
  `$VALUES`), or in how reflection retrieves/caches the universe.

## What We Changed (chronological high‑level)

### DCE area
- Added `*enable-dce*` and env integration (LDK_DCE) for build-time control.
- Fixed DCE read‑tracking to be keyed by var-number sets (not EQ identity),
  eliminating false dead assignments; DCE now default ON.

### Throwable/array safety
- Initialized `Throwable.UNASSIGNED_STACK` to an empty STE[] during load.
- Corrected String construction in array bounds paths (`jstring` usage).
- Added jaref/(setf jaref) diagnostics: print index and length on AIOOBE.
- Hardened `System.arraycopy` (range checks + tracing under debug).

### rt.jar priority and shims
- Removed Lisp shims for `java/lang/Enum` and
  `java/lang/Class.getEnumConstants(Shared)` to prioritize rt.jar code.
- Verified that `Class.getEnumConstants0()` (native) was not called in this
  workload (explains why older OpenLDK never complained about it missing).

### Targeted instrumentation
- Enabled default `*debug-trace*` and `*debug-x*` and `*debug-trace-args*` in source
  to capture function entry/exit and return values.
- Logged `Option.values()` length and `Class.getEnumConstantsShared()` result length.
- (In-progress) Added Option-specific probes to print each `<init>`
  (name, ordinal) and `$VALUES` length after `<clinit>`; adjusting wrappers
  to avoid conflicts with inner subclasses during compile.

## Current State

- Reproducible confirmation:
  - `Option.values()` returns 61
  - `Class.getEnumConstantsShared(Option)` returns 61
  - AIOOBE at ordinal 63 against length 61
- Conclusion: One `Option` enum constant is not present in the values/universe
  array when javac starts using EnumSet. This is an enum init/reflection issue,
  not a math/bit-iteration error.

## Next Steps (short list)

1) Complete the Option constructor/clinit probes (around methods only) and run
   javac to print:
   - Every `Option.<init>` with name/ordinal
   - `$VALUES` length immediately after `Option.<clinit>`
   → Identify the missing ordinal/name and whether it’s absent or NIL in `$VALUES`.

2) Fix root cause (likely one of):
   - Ensure all static enum constant fields initialize in `Option.<clinit>`
     before any reflection path reads `$VALUES` (init ordering).
   - Verify emission doesn’t accidentally shadow or skip one constant for
     nested/anonymous variants (e.g., `Option$N`).
   - If reflection path is reading `$VALUES` early, ensure we don’t call it
     before `Option.<clinit>` completes for the whole enum class.

3) Re-run javac sanity check:
   - Confirm `Option.values()` and `getEnumConstantsShared(Option)` both return 62
     and AIOOBE disappears.

## Commands used during investigation

- Build + run javac:
  - `make clean && make`
  - `LDK_DEBUG=Tx CLASSPATH=$JAVA_HOME/../lib/tools.jar ./openldk \
     com.sun.tools.javac.Main -d out -source 8 -target 8 src/Hello.java`

- Inspect classfile:
  - `javap -classpath $JAVA_HOME/../lib/tools.jar -verbose com.sun.tools.javac.main.Option`

## Notes

- The presence of repeated `Long.numberOfTrailingZeros` returning 20..31..63 in
  traces matches EnumSet’s bit scanning pattern and is not suspicious.
- The crux is making the reflection/values universe dense and complete in our
  runtime for large enums (like javac’s `Option`).

