/* { dg-output "OK" } */

package aaa;

// A catch handler that falls through to code also reached another way --
// here, a try/catch inside an IF whose false branch skips to the same
// statement -- must continue at that statement.  The handler's last block
// was emitted without a jump there, so the method returned null.
public class catchjoin
{
    static Object token(String s)
    {
        StringBuffer buff = new StringBuffer(s);
        char c = s.charAt(0);
        if (c == '+' || c == '-' || (c >= '0' && c <= '9')) {
            try { return new Double(buff.toString()); }
            catch (NumberFormatException e) { ; }
        }
        return buff.toString().toLowerCase().intern();
    }

    static int count(String s)
    {
        int n = 0;
        if (s != null) {
            try { n = Integer.parseInt(s); }
            catch (NumberFormatException e) { n = -1; }
        }
        return n + 100;
    }

    public static void main(String[] args)
    {
        String got = token("+") + " " + token("-x") + " " + token("5") + " " + token("*")
            + " " + count("7") + " " + count("x") + " " + count(null);
        System.out.println(got.equals("+ -x 5.0 * 107 99 100") ? "OK" : "FAIL: " + got);
    }
}
