// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.util.*;
import java.util.function.*;

public class PrimitiveMapperProbe {
    public static Character boxedChar(int value) { return (char) value; }
    public static class Delayed {
        static { System.setProperty("mapper.delayed.initialized", "yes"); }
        final int value;
        public Delayed(int value) { this.value = value; }
    }

    @SuppressWarnings({"unchecked", "removal", "deprecation"})
    public static void main(String[] args) {
        var values = new ArrayList<Integer>();
        values.add(11);
        values.add(23);
        int[] result = values.stream().mapToInt(Function.<Integer>identity()::apply).toArray();
        if (!Arrays.equals(result, new int[]{11, 23})) throw new AssertionError("Stream mapper");
        ToIntFunction<Integer> constructor = Integer::new;
        if (constructor.applyAsInt(17) != 17) throw new AssertionError("Constructor result unboxing");
        IntFunction<Delayed> delayed = Delayed::new;
        if (System.getProperty("mapper.delayed.initialized") != null)
            throw new AssertionError("Constructor capture initialized its class");
        if (delayed.apply(7).value != 7 || !"yes".equals(System.getProperty("mapper.delayed.initialized")))
            throw new AssertionError("Constructor invocation missed class initialization");
        Function<Long, Long> longs = Function.identity();
        ToLongFunction<Long> unboxLong = longs::apply;
        if (unboxLong.applyAsLong(1234567890123L) != 1234567890123L)
            throw new AssertionError("Long unboxing");
        ToDoubleFunction<Integer> widen = Integer::intValue;
        if (widen.applyAsDouble(23) != 23.0) throw new AssertionError("Primitive widening");
        IntUnaryOperator chars = PrimitiveMapperProbe::boxedChar;
        if (chars.applyAsInt(955) != 955) throw new AssertionError("Character widening");
        Function<Integer, Boolean> booleans = value -> value > 0;
        Predicate<Integer> predicate = booleans::apply;
        if (!predicate.test(1) || predicate.test(0)) throw new AssertionError("Boolean unboxing");
        Function<Integer, Integer> nulls = value -> null;
        ToIntFunction<Integer> nullMapper = nulls::apply;
        try { nullMapper.applyAsInt(0); throw new AssertionError("Null unboxing succeeded"); }
        catch (NullPointerException expected) { }
        Function<Integer, Integer> wrong = (Function) (Function<Integer, Object>) (value -> "wrong");
        ToIntFunction<Integer> badMapper = wrong::apply;
        try { badMapper.applyAsInt(0); throw new AssertionError("Invalid unboxing succeeded"); }
        catch (ClassCastException expected) { }
        System.out.println("Primitive SAM return unboxing, widening, null, and type checks passed");
    }
}
