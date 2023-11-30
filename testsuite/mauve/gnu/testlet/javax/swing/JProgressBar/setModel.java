/* setModel.java -- some checks for the setModel()
       method in the JProgressBar class.
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.JProgressBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JProgressBar;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class setModel implements Testlet, ChangeListener
{
  ChangeEvent lastEvent;
  
  public void stateChanged(ChangeEvent e) 
  {
    lastEvent = e;
  }

  public void test(TestHarness harness)
  {
    DefaultBoundedRangeModel m1 = new DefaultBoundedRangeModel(1, 2, 0, 10);
    JProgressBar pb = new JProgressBar(m1);
    pb.addChangeListener(this);
    harness.check(m1.getExtent(), 0);
    harness.check(m1.getChangeListeners().length, 1);
    DefaultBoundedRangeModel m2 = new DefaultBoundedRangeModel(10, 20, 0, 100);
    pb.setModel(m2);
    harness.check(pb.getModel(), m2);
    harness.check(m2.getExtent(), 0);
    harness.check(m1.getChangeListeners().length, 0);
    harness.check(m2.getChangeListeners().length, 1);
    harness.check(lastEvent.getSource(), pb);
    
    boolean pass = false;
    try
    {
      pb.setModel(null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
