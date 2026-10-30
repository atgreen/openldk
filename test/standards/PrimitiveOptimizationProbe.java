// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class PrimitiveOptimizationProbe {
    static long digest = 1;
    static long field;
    static int effects;
    static void take(long x) { digest = digest * 31 + x; }
    static long arithmetic(long x, long y) { return (x * 3 + y) ^ ((x - y) | (x & y)); }
    static long loop(long x, int n) { for (int i=0; i<n; i++) x=x*3+1; return x; }
    static int compare(long x, long y) { return x==y ? 0 : x<y ? -1 : 1; }
    static long div(long x, long y) { return x/y; }
    static long rem(long x, long y) { return x%y; }
    static long effect(long x) { effects++; return x; }
    static void check(boolean ok) { if (!ok) throw new AssertionError(); }
    static long roundtrip(long x) { field=x; long[] a={field}; return a[0]; }
    public static void main(String[] args) {
        long[] values={0,1,-1,Long.MAX_VALUE,Long.MIN_VALUE,1L<<62,-(1L<<62),1234567890123456789L};
        for (long x: values) {
            check(roundtrip(x)==x);
            take(loop(x,100));
            for (long y: values) {
                take(arithmetic(x,y)); take(compare(x,y)); take(-x);
                if (y!=0) { take(div(x,y)); take(rem(x,y)); }
                for (int k: new int[]{-65,-1,0,1,31,32,63,64,65}) {
                    take(x<<k); take(x>>k); take(x>>>k);
                }
                take((int)x); take((long)(float)x); take((long)(double)x);
            }
        }
        for (double d: new double[]{Double.NaN,Double.POSITIVE_INFINITY,Double.NEGATIVE_INFINITY,-0.0,1.75,-1.75}) take((long)d);
        check(div(Long.MIN_VALUE,-1)==Long.MIN_VALUE);
        check(rem(Long.MIN_VALUE,-1)==0);
        check(compare(Long.MIN_VALUE,0)<0);
        check(((-1L)>>>63)==1);
        try { long unused=effect(7)/effect(0); throw new AssertionError(); }
        catch (ArithmeticException expected) { check(effects==2); }
        check(loop(Long.MIN_VALUE,0)==Long.MIN_VALUE);
        System.out.println("primitive optimization passed " + digest);
    }
}
