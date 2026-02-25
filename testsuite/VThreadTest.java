public class VThreadTest {
    public static void main(String[] args) throws Exception {
        // Test 1: Thread.startVirtualThread
        Thread t1 = Thread.startVirtualThread(() -> {
            System.out.println("Hello from virtual thread 1!");
        });
        t1.join();

        // Test 2: Thread.ofVirtual().start()
        Thread t2 = Thread.ofVirtual().name("my-vthread").start(() -> {
            System.out.println("Hello from virtual thread 2!");
        });
        t2.join();

        // Test 3: Multiple virtual threads (started concurrently)
        int n = 10;
        Thread[] threads = new Thread[n];
        for (int i = 0; i < n; i++) {
            final int id = i;
            threads[i] = Thread.startVirtualThread(() -> {
                System.out.println("Virtual thread " + id);
            });
        }
        for (int i = 0; i < n; i++) {
            threads[i].join();
        }

        System.out.println("All virtual threads completed!");
    }
}
