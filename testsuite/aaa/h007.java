// ===== Test Case h007: If-Else Control Flow =====
/* { dg-output "^It's positive.$" } */
package aaa;

public class h007 {
    public static void main(String[] args) {
        int number = 5;
        if (number > 0) {
            System.out.println("It's positive.");
        } else {
            System.out.println("It's negative or zero.");
        }
    }
}
