/* addVetoableChangeListener.java -- some checks for the 
       addVetoableChangeListener() method in the VetoableChangeSupport class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: 1.4

package gnu.testlet.java.beans.VetoableChangeSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.beans.VetoableChangeSupport;

public class addVetoableChangeListener 
  implements Testlet, VetoableChangeListener
{
  public void vetoableChange(PropertyChangeEvent e) 
    throws PropertyVetoException 
  {
  }

  public void test(TestHarness harness)
  {
    testMethod1(harness);
    testMethod2(harness);
  }
  
  public void testMethod1(TestHarness harness)
  {
    harness.checkPoint("(VetoableChangeListener)");
    VetoableChangeSupport vcs = new VetoableChangeSupport(this);
    harness.check(vcs.getVetoableChangeListeners().length, 0);
    vcs.addVetoableChangeListener(this);
    harness.check(vcs.getVetoableChangeListeners().length, 1);
    vcs.addVetoableChangeListener(null);
    harness.check(vcs.getVetoableChangeListeners().length, 1);
  }

  public void testMethod2(TestHarness harness)
  {
    harness.checkPoint("(String, VetoableChangeListener)");
    VetoableChangeSupport vcs = new VetoableChangeSupport(this);
    harness.check(vcs.getVetoableChangeListeners().length, 0);
    vcs.addVetoableChangeListener("A", this);
    vcs.addVetoableChangeListener("B", this);
    harness.check(vcs.getVetoableChangeListeners().length, 2);
    harness.check(vcs.getVetoableChangeListeners("A").length, 1);
    harness.check(vcs.getVetoableChangeListeners("B").length, 1);
    vcs.addVetoableChangeListener("B", null);
    harness.check(vcs.getVetoableChangeListeners().length, 2);
    harness.check(vcs.getVetoableChangeListeners("B").length, 1);

    // try null property name
    vcs.addVetoableChangeListener(null, this);
    harness.check(vcs.getVetoableChangeListeners().length, 2);
    harness.check(vcs.getVetoableChangeListeners("A").length, 1);
    harness.check(vcs.getVetoableChangeListeners("B").length, 1);
  }

}
