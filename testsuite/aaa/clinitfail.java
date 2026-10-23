/* { dg-output "OK" } */

package aaa;

import java.util.*;

// A class whose <clinit> throws is erroneous (JLS 12.4.2): the first use
// throws ExceptionInInitializerError (or the Error itself), every later use
// NoClassDefFoundError.  H2's DbException failing this way once and then
// "working" half-initialized hid an unrelated bug.
public class clinitfail
{
    static int zero() { return 0; }

    static class Bad { static int x = 1 / zero(); static int get() { return x; } }
    static class ByName { static int x = 1 / zero(); }
    static class BadError { static int x; static { if (zero() == 0) throw new AssertionError("boom"); } }

    interface Use { void run() throws Throwable; }

    static String attempt(Use use)
    {
        try {
            use.run();
            return "no exception";
        } catch (Throwable t) {
            String s = t.getClass().getSimpleName();
            if (t.getMessage() != null)
                s += "(" + t.getMessage() + ")";
            if (t.getCause() != null)
                s += "<-" + t.getCause().getClass().getSimpleName();
            return s;
        }
    }

    public static void main(String[] args)
    {
        List<String> got = new ArrayList<>();
        got.add(attempt(() -> Bad.get()));
        got.add(attempt(() -> Bad.get()));
        got.add(attempt(() -> { Bad.x = 2; }));
        got.add(attempt(() -> Class.forName("aaa.clinitfail$ByName")));
        got.add(attempt(() -> System.out.print(ByName.x)));
        got.add(attempt(() -> System.out.print(BadError.x)));
        got.add(attempt(() -> System.out.print(BadError.x)));
        String expected = "[ExceptionInInitializerError<-ArithmeticException, "
            + "NoClassDefFoundError(Could not initialize class aaa.clinitfail$Bad)<-ExceptionInInitializerError, "
            + "NoClassDefFoundError(Could not initialize class aaa.clinitfail$Bad)<-ExceptionInInitializerError, "
            + "ExceptionInInitializerError<-ArithmeticException, "
            + "NoClassDefFoundError(Could not initialize class aaa.clinitfail$ByName)<-ExceptionInInitializerError, "
            + "AssertionError(boom), "
            + "NoClassDefFoundError(Could not initialize class aaa.clinitfail$BadError)<-ExceptionInInitializerError]";
        System.out.println(expected.equals(got.toString()) ? "OK" : got.toString());
    }
}
