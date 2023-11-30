// Tags: JDK1.0

// This test is from Jeff Sturm.
// It tests whether close() on a PipedInputStream will correctly
// notify the writer.

package gnu.testlet.java.io.PipedStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class close implements Runnable, Testlet {
	Thread main;
	PipedInputStream in;
	PipedOutputStream out;
	TestHarness harness;

	public void run() {
		try {
			Thread.sleep(1000);
			harness.debug("Closing pipe input stream:");
			in.close();
			Thread.sleep(1000);
			harness.debug("Interrupting pipe reader:");
			main.interrupt();
		} catch (Throwable t) {
			harness.debug(t);
		}
	}

	public void test (TestHarness harness) {
		int val = 23;
		try {
			close test = new close();
			test.harness = harness;

			test.main = Thread.currentThread();
			test.out = new PipedOutputStream();
			test.in = new PipedInputStream(test.out);

			(new Thread(test)).start();

			val = test.in.read();
		} catch (InterruptedIOException t) {
			harness.check(true,"read() interrupted okay");
		} catch (IOException t) {
			harness.fail("Unexpected IOException thrown");
		}
	}
}
