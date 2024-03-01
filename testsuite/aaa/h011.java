// ===== Test Case h011: Class and Object =====
/* { dg-output "^Dog's name: Buddy$" } */
package aaa;

class Dog {
    String name;
    public Dog(String name) {
        this.name = name;
    }
}

public class h011 {
    public static void main(String[] args) {
        Dog myDog = new Dog("Buddy");
        System.out.println("Dog's name: " + myDog.name);
    }
}
