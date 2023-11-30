/* setValue.java -- some checks for the setValue()
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

import java.util.List;

import javax.swing.DefaultBoundedRangeModel;
import javax.swing.JProgressBar;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class setValue implements Testlet, ChangeListener
{
  List events = new java.util.ArrayList();
  
  public void stateChanged(ChangeEvent e) 
  {
    events.add(e);
  }

  public void test(TestHarness harness) 
  {
    DefaultBoundedRangeModel m = new DefaultBoundedRangeModel();
    m.addChangeListener(this);
    JProgressBar pb = new JProgressBar(m);
    pb.addChangeListener(this);
    harness.check(pb.getValue(), 0);
    pb.setValue(55);
    harness.check(pb.getValue(), 55);
    harness.check(m.getValue(), 55);
    harness.check(events.size(), 2);
    
    // setting the same value triggers no events
    events.clear();
    pb.setValue(55);
    harness.check(events.size(), 0);
    
    // try value < minimum
    events.clear();
    pb.setValue(-1);
    harness.check(pb.getValue(), 0);
    harness.check(m.getValue(), 0);
    harness.check(events.size(), 2);

    // try value > maximum
    events.clear();
    pb.setValue(101);
    harness.check(pb.getValue(), 100);
    harness.check(m.getValue(), 100);
    harness.check(events.size(), 2);
  }
  
}
