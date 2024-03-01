// ===== Test Case h014: Exception Handling =====
/* { dg-output "^Exception: / by zero$" } */
package aaa;

public class h014 {
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            System.out.println("Exception: " + e.getMessage());
        }
    }
}
