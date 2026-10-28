// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
import java.lang.annotation.*;
import java.lang.reflect.*;
import java.util.*;

public class RecordProbe {
    @Retention(RetentionPolicy.RUNTIME) @Target(ElementType.RECORD_COMPONENT)
    @interface Tag { String value(); }
    record Empty() {}
    record LongValue(long value) {}
    record FloatValue(float value) {}
    record DoubleValue(double value) {}
    record BooleanValue(boolean value) {}
    record RefValue(Object value) {}
    record Bytes(byte b, short s, char c, int i) {}
    record Generic<T>(@Tag("component") T value, List<String> names) {}
    record Mixed(boolean flag, long number, float single, double wide, Object ref) {}
    static void check(boolean ok, String message) { if (!ok) throw new AssertionError(message); }
    public static void main(String[] args) throws Exception {
        for (long value : new long[]{0, 1, -1, 0x100000000L, Long.MIN_VALUE, Long.MAX_VALUE})
            check(new LongValue(value).hashCode() == Long.hashCode(value), "long hash " + value);
        for (boolean value : new boolean[]{true, false})
            check(new BooleanValue(value).hashCode() == Boolean.hashCode(value), "boolean hash");
        for (float value : new float[]{0f, -0f, 1.25f, Float.NaN, Float.intBitsToFloat(0x7fc00001), Float.POSITIVE_INFINITY})
            check(new FloatValue(value).hashCode() == Float.hashCode(value), "float hash");
        for (double value : new double[]{0d, -0d, 1.25d, Double.NaN, Double.longBitsToDouble(0x7ff8000000000001L), Double.NEGATIVE_INFINITY})
            check(new DoubleValue(value).hashCode() == Double.hashCode(value), "double hash");
        check(!new FloatValue(0f).equals(new FloatValue(-0f)), "float signed zero");
        check(!new DoubleValue(0d).equals(new DoubleValue(-0d)), "double signed zero");
        check(new FloatValue(Float.NaN).equals(new FloatValue(Float.intBitsToFloat(0x7fc00001))), "float NaNs");
        check(new DoubleValue(Double.NaN).equals(new DoubleValue(Double.longBitsToDouble(0x7ff8000000000001L))), "double NaNs");
        int[] array = {1, 2};
        check(new RefValue(array).hashCode() == array.hashCode(), "array identity hash");
        check(new RefValue(array).equals(new RefValue(array)), "same array");
        check(!new RefValue(array).equals(new RefValue(new int[]{1, 2})), "different arrays");
        check(new RefValue(new String("same")).equals(new RefValue(new String("same"))), "object equality");
        check(new RefValue(null).equals(new RefValue(null)) && new RefValue(null).hashCode() == 0, "null reference");
        check(!new RefValue(null).equals(null) && !new RefValue(null).equals(new LongValue(0)), "record class identity");
        Bytes bytes = new Bytes((byte)-7, (short)-32000, '\uFFFF', -17);
        int expectedBytes = (((-7 * 31) - 32000) * 31 + 65535) * 31 - 17;
        check(bytes.hashCode() == expectedBytes, "narrow components");
        Mixed mixed = new Mixed(true, 0x100000000L, -0f, Double.NaN, "test");
        int hash = Boolean.hashCode(true);
        hash = 31 * hash + Long.hashCode(0x100000000L);
        hash = 31 * hash + Float.hashCode(-0f);
        hash = 31 * hash + Double.hashCode(Double.NaN);
        hash = 31 * hash + "test".hashCode();
        check(mixed.hashCode() == hash, "mixed hash");
        check(new Empty().equals(new Empty()) && new Empty().hashCode() == 0, "empty semantics");
        check(Empty.class.isRecord() && Empty.class.getRecordComponents().length == 0, "empty reflection");
        check(!String.class.isRecord() && String.class.getRecordComponents() == null, "ordinary class reflection");
        check(!int.class.isRecord() && int.class.getRecordComponents() == null, "primitive reflection");
        check(!Empty[].class.isRecord() && Empty[].class.getRecordComponents() == null, "array reflection");
        RecordComponent[] components = Generic.class.getRecordComponents();
        check(components.length == 2 && components[0].getName().equals("value") && components[1].getType() == List.class, "component order/types");
        check(components[0].getGenericType().getTypeName().equals("T"), "generic component");
        check(components[1].getGenericType().getTypeName().equals("java.util.List<java.lang.String>"), "parameterized component");
        check(components[0].getAnnotation(Tag.class).value().equals("component"), "component annotation");
        check(components[0].getDeclaringRecord() == Generic.class, "declaring record");
        check(components[0].getAccessor().invoke(new Generic<>("ok", List.of())).equals("ok"), "component accessor");
        check(new BooleanValue(true).toString().equals("BooleanValue[value=true]"), "boolean string");
        check(new Empty().toString().equals("Empty[]"), "empty string");
        System.out.println("Records: primitive/reference semantics, floating edges, empty records, and reflective metadata passed");
    }
}
