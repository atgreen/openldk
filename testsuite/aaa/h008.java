// ===== Test Case h008: For Loop =====
/* { dg-output "^Sum: 55$" } */
package aaa;

public class h008 {
    public static void main(String[] args) {
        int sum = 0;
        for (int i = 1; i <= 10; i++) {
            sum += i;
        }
        System.out.println("Sum: " + sum);
    }
}
