/* getChangeListeners.java -- Checks for the getChangeListeners() method in the
                              JSpinner class.
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

package gnu.testlet.javax.swing.JSpinner;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Arrays;
import java.util.List;

import javax.swing.JSpinner;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class getChangeListeners implements Testlet, ChangeListener 
{
  List events = new java.util.ArrayList();
  
  public void stateChanged(ChangeEvent event) 
  {
    this.events.add(event);    
  }
  
  public void test(TestHarness harness)
  {
    harness.checkPoint("()");
    JSpinner s = new JSpinner();
    ChangeListener[] cl = s.getChangeListeners();
    harness.check(cl.length, 1);
    harness.check(cl[0], s.getEditor());
    
    s.addChangeListener(this);
    cl = s.getChangeListeners();
    harness.check(cl.length, 2);
    harness.check(Arrays.asList(cl).contains(this));
    
    s.removeChangeListener(this);    
    cl = s.getChangeListeners();
    harness.check(cl.length, 1);
    harness.check(!Arrays.asList(cl).contains(this));
  }
}
