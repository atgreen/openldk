public class Hello
{
    public static int x = 1;

    public static void test() throws java.lang.ArithmeticException
    {
        x = 11/x;
    }

    public static void main(String[] args)
    {
        // System.out.println ("Hello World!");
        try {
            x = 10 / x ;
                //                throw new java.lang.ArithmeticException("Trying to divide by 0");
        } catch (Exception e) {
            System.out.println ("OK!");
        }
        System.out.println ("Done.");
    }
}
