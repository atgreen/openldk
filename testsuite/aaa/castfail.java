/* { dg-output "OK" } */

package aaa;

// A failed cast throws ClassCastException even when nothing has loaded that
// class yet: here the caller only catches RuntimeException.
public class castfail
{
    static Integer f(Object o) { return (Integer) o; }

    public static void main(String[] args)
    {
        try {
            System.out.println("FAIL: no exception " + f("x"));
        } catch (RuntimeException e) {
            System.out.println(e.getClass().getName().equals("java.lang.ClassCastException")
                               ? "OK" : "FAIL: " + e);
        }
    }
}
