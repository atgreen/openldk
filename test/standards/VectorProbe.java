// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import jdk.incubator.vector.*;
import java.util.Arrays;
public class VectorProbe {
    static void check(boolean v) { if (!v) throw new AssertionError(); }
    public static void main(String[] args) {
        var s = IntVector.SPECIES_128;
        int[] a = {Integer.MAX_VALUE, -2, 3, 4};
        int[] b = {1, 5, -6, 7};
        var x = IntVector.fromArray(s, a, 0);
        var y = IntVector.fromArray(s, b, 0);
        int[] out = new int[4]; x.add(y).intoArray(out, 0);
        check(Arrays.equals(out, new int[]{Integer.MIN_VALUE, 3, -3, 11}));
        check(x.compare(VectorOperators.GT, 0).trueCount() == 3);
        check(y.reduceLanes(VectorOperators.ADD) == 7);
        x.rearrange(VectorShuffle.fromArray(s, new int[]{3,2,1,0}, 0)).intoArray(out, 0);
        check(Arrays.equals(out, new int[]{4,3,-2,Integer.MAX_VALUE}));
        x.add(y, VectorMask.fromLong(s, 5)).intoArray(out, 0);
        check(Arrays.equals(out, new int[]{Integer.MIN_VALUE,-2,-3,4}));
        float[] f = {1.0f, -0.0f, 3.5f, -4.0f};
        check(FloatVector.fromArray(FloatVector.SPECIES_128, f, 0).mul(2).lane(2) == 7.0f);
        check(ByteVector.broadcast(ByteVector.SPECIES_128, (byte)120).add((byte)10).lane(15) == (byte)-126);
        check(ShortVector.broadcast(ShortVector.SPECIES_128, Short.MAX_VALUE).add((short)1).lane(7) == Short.MIN_VALUE);
        check(LongVector.broadcast(LongVector.SPECIES_128, Long.MAX_VALUE).add(1).lane(1) == Long.MIN_VALUE);
        check(Double.doubleToRawLongBits(DoubleVector.broadcast(DoubleVector.SPECIES_128, -0.0d).mul(2).lane(1)) == Long.MIN_VALUE);
        int[] tail = {1, 2, 3};
        var mask = s.indexInRange(0, tail.length);
        check(IntVector.fromArray(s, tail, 0, mask).reduceLanes(VectorOperators.ADD) == 6);
        int[] map = {3, 1, 0, 2};
        IntVector.fromArray(s, a, 0, map, 0).intoArray(out, 0, map, 0);
        check(Arrays.equals(a, out));
        check(ModuleLayer.boot().findModule("jdk.incubator.vector").orElseThrow() == IntVector.class.getModule());
        System.out.println("Vectors: arithmetic, masks, shuffles, reductions and arrays passed");
    }
}
