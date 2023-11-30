/* setStart.java -- some checks for the setStart() method in the
                    SpinnerDateModel class.
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

package gnu.testlet.javax.swing.SpinnerDateModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Calendar;
import java.util.Date;

import javax.swing.SpinnerDateModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class setStart implements Testlet, ChangeListener {
  
  private ChangeEvent event;
  
  public void stateChanged(ChangeEvent e)
  {
    this.event = e;    
  }
  
  public void test(TestHarness harness) 
  {
    Date now = new Date();
    Date start1 = new Date(now.getTime() - 1L);
    Date start2 = new Date(now.getTime() - 2L);
    Date end = new Date(now.getTime() + 1L);
    Date postEnd = new Date(now.getTime() + 2L);
    
    SpinnerDateModel m = new SpinnerDateModel(now, start1, end, 
            Calendar.DAY_OF_MONTH);
    m.addChangeListener(this);
    m.setStart(start2);
    harness.check(m.getStart(), start2);
    harness.check(this.event.getSource(), m);
    
    // same value triggers no event
    this.event = null;
    m.setStart(start2);
    harness.check(this.event == null);
    
    // set null start
    this.event = null;
    m.setStart(null);
    harness.check(m.getStart(), null);
    harness.check(this.event.getSource(), m);

    // same null value triggers no event
    this.event = null;
    m.setStart(null);
    harness.check(this.event == null);
    
    // set start higher than end
    m.setStart(postEnd);
    harness.check(m.getStart(), postEnd);
  }
}
