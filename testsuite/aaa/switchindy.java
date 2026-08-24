/* { dg-output "A7\r\nB42\r\nC99" } */

package aaa;

// Regression test: a type/enum switch whose LAST case body ends in an
// invokedynamic (here makeConcatWithConstants string concat) used to fall off
// the method and return null, because the INVOKEDYNAMIC transpiler failed to
// register its fall-through successor (bd ldk-1py). The last case's merge/
// return block is emitted early via case-0's goto, so the missing fall-through
// edge left the last case with no terminating GO.
public class switchindy
{
    sealed interface I permits A, B, C {}
    record A(int x) implements I {}
    record B(long d) implements I {}
    record C(double c) implements I {}

    static String describe(I i)
    {
        return switch (i) {
            case A a -> "A" + a.x();
            case B b -> "B" + b.d();
            case C c -> "C" + (long) c.c();
        };
    }

    public static void main(String[] args)
    {
        System.out.println(describe(new A(7)));
        System.out.println(describe(new B(42L)));
        System.out.println(describe(new C(99.0)));
    }
}
