/* { dg-output "こんにちは、世界！.*Hola, Mundo!¡" } */

package aaa;

public class h005
{
    public static void main(String[] args)
    {
        String greetingDirect = "こんにちは、世界！"; // "Hello, World!" in Japanese
        String greetingEscape = "Hola, Mundo!\u00A1"; // "Hello, World!" in Spanish, with an inverted exclamation mark

        System.out.println(greetingDirect);
        System.out.println(greetingEscape);
    }
}
