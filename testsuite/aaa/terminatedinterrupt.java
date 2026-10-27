/* { dg-output "OK" } */
package aaa;
public class terminatedinterrupt {
    public static void main(String[] args) throws Exception {
        for (int i = 0; i < 100; i++) {
            Thread thread = new Thread(() -> {});
            thread.start();
            thread.join();
            thread.interrupt();
        }
        System.out.println("OK");
    }
}
