/* { dg-output "OK" } */

package aaa;

// A loop that keeps a value on the operand stack across its backward
// branch, entered by a forward goto -- which javac never generates, but
// Kawa does (filling an array).  The class is assembled by hand (generated
// by a small script; version 49, so it needs no StackMapTable):
//
//   static Object[] build(int n) {
//      0: iload_0  1: anewarray Object  4: iconst_0  5: istore_1
//      6: goto 16
//      9: dup  10: iload_1  11: aconst_null  12: aastore   ; the array stays on the stack
//     13: iinc 1, 1
//     16: iload_1  17: iload_0  18: if_icmplt 9
//     21: areturn
//   }
//
// OpenLDK transpiled pc 9 with an empty stack: only the later branch at 18
// reaches it.
public class stackloop extends ClassLoader
{
    private static final byte[] LOOP = new byte[] {
        -54,-2,-70,-66,0,0,0,49,0,10,1,0,18,97,97,97,47,115,116,97,99,107,108,111,111,112,36,76,111,111,112,7,0,1,1,0,16,106,97,118,97,47,108,97,110,103,47,79,98,106,101,99,116,7,0,3,1,0,16,106,97,118,97,47,108,97,110,103,47,79,98,106,101,99,116,7,0,5,1,0,4,67,111,100,101,1,0,5,98,117,105,108,100,1,0,22,40,73,41,91,76,106,97,118,97,47,108,97,110,103,47,79,98,106,101,99,116,59,0,33,0,2,0,4,0,0,0,0,0,1,0,9,0,8,0,9,0,1,0,7,0,0,0,34,0,4,0,2,0,0,0,22,26,-67,0,6,3,60,-89,0,10,89,27,1,83,-124,1,1,27,26,-95,-1,-9,-80,0,0,0,0,0,0
    };

    protected Class<?> findClass(String name) throws ClassNotFoundException
    {
        if (!name.equals("aaa.stackloop$Loop")) throw new ClassNotFoundException(name);
        return defineClass(name, LOOP, 0, LOOP.length);
    }

    public static void main(String[] args) throws Exception
    {
        Class<?> loop = new stackloop().loadClass("aaa.stackloop$Loop");
        Object[] built = (Object[]) loop.getMethod("build", int.class).invoke(null, 3);
        System.out.println(built.length == 3 ? "OK" : "FAIL: length " + built.length);
    }
}
