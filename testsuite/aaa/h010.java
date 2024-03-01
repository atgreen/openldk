// ===== Test Case h010: Switch Case =====
/* { dg-output "^Size: Medium$" } */
package aaa;

public class h010 {
    public static void main(String[] args) {
        int sizeCode = 2;
        switch (sizeCode) {
            case 1:
                System.out.println("Size: Small");
                break;
            case 2:
                System.out.println("Size: Medium");
                break;
            case 3:
                System.out.println("Size: Large");
                break;
            default:
                System.out.println("Unknown Size");
        }
    }
}
