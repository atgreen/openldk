// Helper for 

// Copyright (c) 2001  Free Software Foundation

// This file is part of Mauve.

// Tags: not-a-test

package gnu.testlet.java.io.BufferedOutputStream;

import java.io.*;

public class helper extends OutputStream
{
  // Number of bytes we've read.
  int count;
  // When we should stop.
  int stop;

  public helper (int size)
  {
    stop = size;
  }

  private void update (int howmuch) throws InterruptedIOException
  {
    if (count + howmuch > stop)
      {
	InterruptedIOException ioe = new InterruptedIOException ();
	ioe.bytesTransferred = stop - count;
	count = stop;
	throw ioe;
      }

    count += howmuch;
  }

  public void write (int b) throws InterruptedIOException
  {
    update (1);
  }

  public void write (byte[] b, int off, int len) throws InterruptedIOException
  {
    if (off < 0 || len < 0 || off + len > b.length)
      throw new ArrayIndexOutOfBoundsException ();
    update (len);
  }
}
