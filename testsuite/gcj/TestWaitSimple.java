public class TestWaitSimple {
  public static void main(String[] args) throws Exception {
    final Object lock = new Object();
    
    Thread t = new Thread(new Runnable() {
      public void run() {
        synchronized (lock) {
          try {
            System.out.println("Thread waiting...");
            lock.wait(1000);  // Wait with timeout
            System.out.println("Thread woke up!");
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
        }
      }
    });
    
    t.start();
    Thread.sleep(500);
    
    synchronized (lock) {
      System.out.println("Notifying...");
      lock.notify();
    }
    
    t.join(2000);
    System.out.println("Done");
  }
}
