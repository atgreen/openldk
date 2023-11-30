// Test of WeakReference

// Copyright (C) 2001 Red Hat, Inc.

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

// Tags: JDK1.2

// In this test we make some assumptions about how the GC operates
// that are probably not quite sound.  In particular we assume
// System.gc() will collect everything.

package gnu.testlet.java.lang.ref.WeakReference;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.ref.*;

public class weakref implements Testlet
{

  static class Reffer extends Thread
  {
    ReferenceQueue q;
    TestHarness harness;
    WeakReference wr;
    Integer twt;

    public Reffer (ReferenceQueue q, TestHarness harness)
    {
      this.q = q;
      this.harness = harness;
    }

    public void run()
    {
      twt = new Integer (23);
      wr = new WeakReference (twt, q);

      System.gc ();

      Reference r = q.poll ();
      harness.check (r, null, "live reference");
      harness.check (wr.get (), twt);
    }
  }

  public void test (TestHarness harness)
  {
    ReferenceQueue q = new ReferenceQueue ();

    // Create reference in a separate thread so no inadvertent references
    // to the contained object are left on the stack, which causes VM's that
    // do conservative stack GC scans to report false negatives for this test.
    Reffer reffer = new Reffer(q, harness); 
    reffer.start();
    try {
      reffer.join();
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
    WeakReference wr = reffer.wr;

    // Weak reference "should" get cleared here
    reffer.twt = null;
    System.gc ();

    Reference r = null;
    try
      {
	r = q.remove (5 * 1000); // 5 seconds.
      }
    catch (InterruptedException _)
      {
	harness.debug (_);
      }

    harness.check (r, wr, "unreachable");
    harness.check (wr.get (), null, "contents of weak reference");
  }
}
