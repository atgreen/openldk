/* { dg-output "OK" } */

package aaa;

import java.io.File;

// An ldc of a class that isn't there throws NoClassDefFoundError only when
// it runs (Spring's DefaultConversionService refers to kotlin.text.Regex
// behind a check for Kotlin).  The JIT failed to compile the method.
public class ldcmissing
{
    static boolean present = false;

    static Object probe()
    {
        return present ? ldcmissingGone.class : "absent";
    }

    public static void main(String[] args) throws Exception
    {
        new File(ldcmissing.class.getResource("ldcmissingGone.class").toURI()).delete();
        String result = String.valueOf(probe());
        present = true;
        try {
            probe();
            result = "FAIL: no error";
        } catch (NoClassDefFoundError e) {
        }
        System.out.println("absent".equals(result) ? "OK" : result);
    }
}

class ldcmissingGone
{
}
