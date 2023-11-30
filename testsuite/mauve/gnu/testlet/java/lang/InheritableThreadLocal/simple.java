// Simple tests of InheritableThreadLocal

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

// Tags: JDK1.3

package gnu.testlet.java.lang.InheritableThreadLocal;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class simple implements Testlet, Runnable
{
  private class TestInheritableThreadLocal extends InheritableThreadLocal
  {
    public Object initialValue ()
    {
      return "Maude";
    }

    protected Object childValue(Object parentValue)
    {
      myHarness.check (parentValue, "Liver", "Check parent value");
      return "Spice";
    }
  }

  private TestHarness myHarness;
  private TestInheritableThreadLocal loc = new TestInheritableThreadLocal();

  public void run ()
  {
    myHarness.check (loc.get (), "Spice", "Child value in new thread");
    loc.set ("Wednesday");
    myHarness.check (loc.get (), "Wednesday", "Changed value in new thread");
  }

  public void test (TestHarness harness)
  {
    this.myHarness = harness;

    harness.check (loc.get (), "Maude", "Check initial value");

    loc.set ("Liver");
    harness.check (loc.get (), "Liver", "Check changed value");

    try
      {
	Thread t = new Thread(this);
	t.start ();
	t.join ();
      }
    catch (InterruptedException _)
      {
      }

    harness.check (loc.get (), "Liver", "Value didn't change");
  }
}
