// Tests that UTF8 decoder always makes progress.

// Written by Mark Wielaard <mark@klomp.org>
// Based on a test by Patrik Reali <reali@acm.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// Tags: JDK1.1

package gnu.testlet.java.io.InputStreamReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

public class utf8 implements Testlet, Runnable
{
  TestHarness harness;
  boolean ok = false;

  public void test (TestHarness h)
  {
    harness = h;
    Thread t = new Thread(this);
    t.start();

    // Wait a few seconds for the thread to finish.
    try
      {
	t.join(3 * 1000);
      }
    catch (InterruptedException ie)
      {
	harness.debug("Interrupted: " + ie);
      }
    harness.check(ok, "UTF-8 decoder finished");

    if (!ok)
      t.interrupt();
  }

  public void run()
  {
    try
      {
	PipedOutputStream  pos = new PipedOutputStream();
	OutputStreamWriter osw = new OutputStreamWriter(pos, "UTF-8");
	PrintWriter        ps  = new PrintWriter(osw);

	PipedInputStream   pis = new PipedInputStream(pos);
	InputStreamReader  isr = new InputStreamReader(pis, "UTF-8");
	char[]  buf = new char[256];
	int     read;

	ps.print("0123456789ABCDEF");
	ps.flush();

	// Read much more then we actually expect (16 characters).
	read = isr.read(buf, 0, 256);
	harness.check(read, 16, "16 characters read");
        ok = true;
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false, "IOException in run()");
      }
  }
}
