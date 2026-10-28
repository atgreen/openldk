# Floating remainder

Java `frem` and `drem` use specialized Lisp implementations in `src/math.lisp`.
They reduce integer significands modulo the divisor and consume the exponent
gap in bounded chunks. On 64-bit SBCL, 32-bit chunks for float and 9-bit chunks
for double keep intermediates within positive fixnums. The result is rebuilt
from bits, including subnormals and signed zero. Invalid operands produce NaN.
No floating quotient or potentially overflowing product is needed.

The exact rational implementation remains in `test/remainder/reference.lisp`
as an independent test oracle; production images do not load it.

## Reproduce

With dependencies installed and `JAVA_HOME` pointing to JDK 25:

```sh
make check-remainder
make bench-remainder
```

The first command generates 100,529 float and 100,529 double vectors with the
reference JDK, then executes the same remainder bytecode on both runtimes.
Each format includes a 23-by-23 boundary matrix and 100,000 deterministic
random raw-bit pairs. Comparisons preserve signed zeros and canonicalize NaNs;
NaN payload propagation is not required. ASDF's unit tests independently compare
201,058 pairs against exact rational arithmetic.

The Linux benchmark checks both Lisp and libm against the rational oracle,
then measures typed, preallocated loops with a retained result checksum. It
reports the median of three trials and bytes allocated per operation. It also
writes disassembly to `.cache/remainder-test/disassembly.txt`.

## Measurements

On an AMD EPYC virtual CPU, SBCL 2.6.9-1.fc43, Linux x86-64, measured in
nanoseconds per operation:

| Format | Workload | Lisp | libm | Rational |
| --- | --- | ---: | ---: | ---: |
| float | Nearby magnitudes | 14.78 | 9.06 | 244.14 |
| float | Moderate exponent gap | 19.55 | 14.78 | 183.11 |
| float | Wide exponent gap | 78.20 | 111.58 | 2532.93 |
| float | Subnormals | 14.78 | 8.11 | 2838.10 |
| double | Nearby magnitudes | 18.60 | 14.78 | 5859.31 |
| double | Moderate exponent gap | 29.09 | 14.78 | 244.14 |
| double | Wide exponent gap | 1339.90 | 851.15 | 6347.60 |
| double | Subnormals | 28.13 | 13.83 | 8941.59 |

Both Lisp and libm loops allocated zero bytes per operation in these runs.
The rational path allocated about 78–5323 bytes per operation, depending on the
workload. Disassembly of the typed Lisp loops contained integer division and
shifts, with no function calls or allocation paths.

The Lisp implementation is about 5–318 times faster than the previous rational
path on these inputs. libm remains faster in most measured cases; Lisp wins the
wide-gap float case. These are kernel microbenchmarks, not whole-application
speedups. The double wide-gap case remains an optimization opportunity.
