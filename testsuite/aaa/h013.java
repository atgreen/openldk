// ===== Test Case h013: Interface Implementation =====
/* { dg-output "^Animal sound: Meow" } */
package aaa;

interface Animal {
    void sound();
}

class Cat implements Animal {
    public void sound() {
        System.out.println("Animal sound: Meow");
    }
}

public class h013 {
    public static void main(String[] args) {
        Cat myCat = new Cat();
        myCat.sound();
    }
}
