// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class PrimitiveResultProbe {
    static long digest=1;
    static int effects;
    public static int chooseInt(int x) { return (x&1)==0 ? x : -x; }
    public static long chooseLong(long x) { return (x&1)==0 ? x : -x; }
    public static float chooseFloat(float x) { return x<0 ? -x : x; }
    public static double chooseDouble(double x) { return x<0 ? -x : x; }
    public static int ints(int x,int y) { return chooseInt(x)*chooseInt(y)+1; }
    public static long longs(long x,long y) { return chooseLong(x)*chooseLong(y)+1; }
    public static float floats(float x,float y) { return chooseFloat(x)*3+chooseFloat(y); }
    public static double doubles(double x,double y) { return chooseDouble(x)*3+chooseDouble(y); }
    public static int afterInt(int x) { return chooseInt(x)+1; }
    public static int afterIntBaseline(int x) { return chooseInt(x)+1; }
    public static int passInt(int x) { return chooseInt(x); }
    static int effect(int x) { effects=effects*10+x; return x; }
    public static void clearEffects() { effects=0; }
    public static int effectCounter() { return effects; }
    public static int order() { return chooseInt(effect(1))+chooseInt(effect(2)); }
    public static int orderBaseline() { return chooseInt(effect(1))+chooseInt(effect(2)); }
    static void record(long value) { digest=digest*31+value; }
    public static void main(String[] args) {
        int[] iv={0,1,-1,Integer.MIN_VALUE,Integer.MAX_VALUE,65535};
        long[] lv={0,1,-1,Long.MIN_VALUE,Long.MAX_VALUE,1L<<62};
        double[] dv={0,-0.0,1.5,-2.25,Double.MIN_VALUE,Double.MAX_VALUE,Double.NaN,
                     Double.POSITIVE_INFINITY,Double.NEGATIVE_INFINITY};
        for(int a:iv) for(int b:iv) record(ints(a,b));
        for(long a:lv) for(long b:lv) record(longs(a,b));
        for(double a:dv) for(double b:dv) {
            record(Double.doubleToLongBits(doubles(a,b)));
            record(Float.floatToIntBits(floats((float)a,(float)b)));
        }
        record(afterInt(7)); record(passInt(7)); record(order());
        if(effects!=12) throw new AssertionError("operand order");
        System.out.println("primitive results passed "+digest);
    }
}
