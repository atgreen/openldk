// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class ArrayLoopProbe {
    static long digest = 1;
    static int effects;
    public static int sum(int[] a) { int s=0; for(int i=0;i<a.length;++i) s+=a[i]; return s; }
    public static long longs(long[] a) { long s=0; for(int i=0;i<a.length;++i) s+=a[i]; return s; }
    public static double dot(double[] a,double[] b) { double s=0; for(int i=0;i<a.length;++i) s+=a[i]*b[i]; return s; }
    public static int pair(int[] a,int[] b) { int s=0; for(int i=0;i<a.length;++i) s+=b[i]; return s; }
    public static int suffix(int[] a,int[] b) { int s=0, i=0; for(;i<a.length;++i) s+=b[i]; return s+b[i]; }
    public static int prefix(int[] a,int[] b) { int s=b[0]; for(int i=0;i<a.length;++i) s+=b[i]; return s; }
    public static int observed(int[] a,int[] b) { int s=0; for(int i=0;i<a.length;++i) { ++effects; s+=b[i]; } return s; }
    static void record(long value) { digest = digest*31+value; }
    static void check(int expected, int actual) { if(expected!=actual) throw new AssertionError(expected+" != "+actual); }
    public static void main(String[] args) {
        int[] sizes={0,1,2,3,5,17,100};
        for(int n:sizes) {
            int[] a=new int[n]; long[] l=new long[n]; double[] d=new double[n];
            for(int i=0;i<n;++i) { a[i]=Integer.MAX_VALUE-i; l[i]=Long.MAX_VALUE-i; d[i]=i*0.5; }
            record(sum(a)); record(longs(l)); record(Double.doubleToLongBits(dot(d,d)));
            for(int m:sizes) {
                int[] b=new int[m]; double[] e=new double[m];
                for(int i=0;i<m;++i) { b[i]=i+7; e[i]=i*1.5; }
                try { record(pair(a,b)); if(m<n) throw new AssertionError("missing pair bounds"); }
                catch(ArrayIndexOutOfBoundsException ex) { if(m>=n) throw ex; record(-1); }
                try { record(Double.doubleToLongBits(dot(d,e))); if(m<n) throw new AssertionError("missing dot bounds"); }
                catch(ArrayIndexOutOfBoundsException ex) { if(m>=n) throw ex; record(-2); }
            }
        }
        check(0,pair(new int[0],null));
        if(dot(new double[0],null)!=0) throw new AssertionError("zero iteration");
        try { sum(null); throw new AssertionError("missing null error"); }
        catch(NullPointerException expected) { record(-3); }
        try { pair(new int[1],null); throw new AssertionError("missing body null error"); }
        catch(NullPointerException expected) { record(-4); }
        try { suffix(new int[0],new int[0]); throw new AssertionError("missing suffix bounds"); }
        catch(ArrayIndexOutOfBoundsException expected) { record(-5); }
        try { prefix(new int[0],new int[0]); throw new AssertionError("missing prefix bounds"); }
        catch(ArrayIndexOutOfBoundsException expected) { record(-6); }
        try { observed(new int[4],new int[2]); throw new AssertionError("missing observed bounds"); }
        catch(ArrayIndexOutOfBoundsException expected) { check(3,effects); }
        System.out.println("array loops passed "+digest);
    }
}
