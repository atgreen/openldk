public class TestWaitThis implements Runnable {
  public static void main(String[] args) throws Exception {
    new TestWaitThis();
  }
  
  public TestWaitThis() {
    System.out.println("creating thread");
    Thread t = new Thread(this);
    t.start();
    
    try {
      Thread.sleep(100);
    } catch (Exception x) {
      System.out.println("exception occurred: " + x);
    }
    
    synchronized (this) {
      System.out.println("notifying other thread");
      notify();
    }
  }
  
  public void run() {
    System.out.println("new thread running");
    synchronized (this) {
      try {
        wait();
      } catch (Exception x) {
        System.out.println("exception occurred: " + x);
      }
    }
    System.out.println("thread notified okay");
  }
}
