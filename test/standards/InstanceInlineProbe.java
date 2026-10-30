// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
public class InstanceInlineProbe {
    public final int add(int a, int b) { return a + b; }
    public final int identity(int a) { return a; }
    public int pass(int a) { return identity(a); }
    private int privateIdentity(int a) { return a; }
    public int privatePass(int a) { return privateIdentity(a); }
    private int subtract(int a, int b) { return a - b; }
    public int privateCall(int a, int b) { return subtract(a,b); }
    public int loop(int n) { int s=0; for(int i=0;i<n;++i) s=add(s,i); return s; }
    public int privateLoop(int n) { int s=0; for(int i=0;i<n;++i) s=subtract(s,i); return s; }
    public static class Child extends InstanceInlineProbe { }
    static final class FinalOwner { int multiply(int a, int b) { return a*b; } }
    static long digest=1;
    static void record(int a) { digest=digest*31+a; }
    public static void main(String[] args) {
        var p=new InstanceInlineProbe();
        var f=new FinalOwner();
        int[] values={0,1,-1,Integer.MIN_VALUE,Integer.MAX_VALUE,65535};
        for(int a:values) for(int b:values) { record(p.add(a,b)); record(p.privateCall(a,b)); record(f.multiply(a,b)); }
        record(p.loop(0)); record(p.loop(1000)); record(p.privateLoop(1000));
        try { ((InstanceInlineProbe)null).add(1,2); throw new AssertionError("missing null receiver error"); }
        catch(NullPointerException expected) { record(-1); }
        System.out.println("instance inline passed "+digest);
    }
}
