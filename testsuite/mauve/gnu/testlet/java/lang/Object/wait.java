/* Copyright (C) 2001 Eric Blake

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.0

package gnu.testlet.java.lang.Object;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * This class tests the trivial exceptions possible with wait, but
 * it does NOT test for InterruptedException, which is only possible
 * with threaded programming.
 */
public final class wait implements Testlet {
  public void test (TestHarness harness)
  {
    // wait must have reasonable args
    try
      {
	wait(-1);
	harness.fail("bad arg not detected");
      }
    catch (IllegalArgumentException iae)
      {
	harness.check(true, "bad arg detected");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.fail("bad arg not detected");
      }
    catch (InterruptedException ie)
      {
	harness.fail("bad arg not detected");
      }
    try
      {
	wait(0, -1);
	harness.fail("bad arg not detected");
      }
    catch (IllegalArgumentException iae)
      {
	harness.check(true, "bad arg detected");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.fail("bad arg not detected");
      }
    catch (InterruptedException ie)
      {
	harness.fail("bad arg not detected");
      }
    try
      {
	wait(0, 1000000);
	harness.fail("bad arg not detected");
      }
    catch (IllegalArgumentException iae)
      {
	harness.check(true, "bad arg detected");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.fail("bad arg not detected");
      }
    catch (InterruptedException ie)
      {
	harness.fail("bad arg not detected");
      }

    // wait and notify must be called in synchronized code
    try
      {
	wait();
	harness.fail("wait called outside synchronized block");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.check(true, "wait called outside synchronized block");
      }
    catch (InterruptedException ie)
      {
	harness.fail("wait called outside synchronized block");
      }
    try
      {
	wait(1);
	harness.fail("wait called outside synchronized block");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.check(true, "wait called outside synchronized block");
      }
    catch (InterruptedException ie)
      {
	harness.fail("wait called outside synchronized block");
      }
    try
      {
	wait(1, 0);
	harness.fail("wait called outside synchronized block");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.check(true, "wait called outside synchronized block");
      }
    catch (InterruptedException ie)
      {
	harness.fail("wait called outside synchronized block");
      }
    try
      {
	notify();
	harness.fail("notify called outside synchronized block");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.check(true, "notify called outside synchronized block");
      }
    try
      {
	notifyAll();
	harness.fail("notifyAll called outside synchronized block");
      }
    catch (IllegalMonitorStateException imse)
      {
	harness.check(true, "notifyAll called outside synchronized block");
      }
  }
}
