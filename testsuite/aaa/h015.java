// ===== Test Case h015: ArrayList Usage =====
/* { dg-output "^List size: 3$" } */
package aaa;

import java.util.ArrayList;

public class h015 {
    public static void main(String[] args) {
        ArrayList<String> list = new ArrayList<>();
        list.add("Java");
        list.add("Python");
        list.add("C++");
        System.out.println("List size: " + list.size());
    }
}
