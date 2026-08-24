/* { dg-output "CHILD TEST(\n|\r\n|\r)BASE TEST" } */

package aaa;

public class TestSuper {
    static class Base {
        public void test() {
            System.out.println("BASE TEST");
        }
    }

    static class Child extends Base {
        public void test() {
            System.out.println("CHILD TEST");
            super.test();
        }
    }

    public static void main(String[] args) {
        new Child().test();
    }
}
