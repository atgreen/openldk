// ===== Test Case h012: Inheritance =====
/* { dg-output "^Vehicle type: Car$" } */
package aaa;

class Vehicle {
    String type;
}

class Car extends Vehicle {
    public Car() {
        this.type = "Car";
    }
}

public class h012 {
    public static void main(String[] args) {
        Car myCar = new Car();
        System.out.println("Vehicle type: " + myCar.type);
    }
}
