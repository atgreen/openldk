// ===== Test Case h009: While Loop =====
/* { dg-output "^Counter: 5$" } */
package aaa;

public class h009 {
    public static void main(String[] args) {
        int counter = 0;
        while (counter < 5) {
            counter++;
        }
        System.out.println("Counter: " + counter);
    }
}
