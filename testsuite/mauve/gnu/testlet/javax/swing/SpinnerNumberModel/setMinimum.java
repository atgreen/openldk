/* setMinimum.java -- some checks for the setMinimum() method in the
                      SpinnerNumberModel class.
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.SpinnerNumberModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class setMinimum implements Testlet, ChangeListener {
  
  private ChangeEvent event;
  
  public void stateChanged(ChangeEvent e)
  {
    this.event = e;    
  }
  
  public void test(TestHarness harness) 
  {
    SpinnerNumberModel m = new SpinnerNumberModel(2.0, 1.0, 3.0, 0.5);
    m.addChangeListener(this);
    m.setMinimum(new Double(0.7));
    harness.check(m.getMinimum(), new Double(0.7));
    harness.check(this.event.getSource(), m);
    
    // same value triggers no event
    this.event = null;
    m.setMinimum(new Double(0.7));
    harness.check(this.event == null);
    
    // set null minimum
    this.event = null;
    m.setMinimum(null);
    harness.check(m.getMinimum(), null);
    harness.check(this.event.getSource(), m);

    // same null value triggers no event
    this.event = null;
    m.setMinimum(null);
    harness.check(this.event == null);
    
    // set minimum higher than maximum
    m.setMinimum(new Double(99.0));
    harness.check(m.getMinimum(), new Double(99.0));
  }
}
