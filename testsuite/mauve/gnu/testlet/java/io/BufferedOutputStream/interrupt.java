// Test to see if InterruptedIOException affects output streams

// Copyright (c) 2001  Free Software Foundation

// This file is part of Mauve.

// Tags: JDK1.1
// Uses: helper

package gnu.testlet.java.io.BufferedOutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class interrupt extends BufferedOutputStream implements Testlet
{
  public interrupt (OutputStream out, int size)
  {
    super (out, size);
  }

  public interrupt ()
  {
    super (null);
  }

  private int getCount()
  {
    return this.count;
  }

  public void test (TestHarness harness)
  {
    // We create an output stream that will throw an
    // InterruptedIOException after 10 bytes are written.  Then we
    // wrap it in a buffered output stream with a buffer that is a bit
    // smaller than that -- but not a nice multiple.  Finally we write
    // bytes until we get the interrupt.

    int BUFFER = 7;

    helper h = new helper (10);
    interrupt out = new interrupt (h, BUFFER);

    boolean ok = false;
    int i = -1;
    int xfer = -1;
    try
      {
	for (i = 0; i < BUFFER * 2; ++i)
	  out.write (i);
	out.flush ();
      }
    catch (InterruptedIOException ioe)
      {
	xfer = ioe.bytesTransferred;
	ok = true;
      }
    catch (IOException _)
      {
      }
    harness.check (ok, "single-byte writes");
    // The flush() will cause the second buffer to be written.  This
    // will only write 3 bytes, though.
    harness.check (xfer, 3);
    harness.check (i, BUFFER * 2);
    // In theory the BufferedOutputStream should notice the
    // InterruptedIOException and update its internal data structure
    // accordingly.
    // harness.check (out.getCount(), 4);

    h = new helper (10);
    out = new interrupt (h, BUFFER);
    byte[] b = new byte[7];

    ok = false;
    xfer = 0;
    try
      {
	for (i = 0; i < 5; ++i)
	  out.write (b);
      }
    catch (InterruptedIOException ioe)
      {
	xfer = ioe.bytesTransferred;
	ok = true;
      }
    catch (IOException _)
      {
      }
    harness.check (ok, "byte array writes");
    harness.check (xfer, 3);
    harness.check (i, 1);
  }
}
