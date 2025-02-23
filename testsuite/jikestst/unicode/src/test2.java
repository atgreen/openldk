/**
 * This code test following cases:
 * 
 * 1. distinction between UNICODE and ASCII white spaces in java parser
 *
 * During compilation it must produce syntax error around line 14
 * 
 * Vadim Zaliva <lord@crocodile.org>
 */
public class test2
{
    public static void main(String []args)
    {
        int res=1\u2029; // U+2029 paragraph separator
        System.out.println("Is whitespace:"+Character.isWhitespace('\u2029'));
    }
};


