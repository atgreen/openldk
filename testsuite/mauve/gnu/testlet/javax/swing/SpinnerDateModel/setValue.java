/* setValue.java -- some checks for the setValue() method in the
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

public class setValue implements Testlet, ChangeListener {
  
  private ChangeEvent event;
  
  public void stateChanged(ChangeEvent e)
  {
    this.event = e;    
  }
  
  public void test(TestHarness harness) 
  {
    Date now = new Date();
    Date v1 = new Date(now.getTime() - 1L);
    Date start = new Date(now.getTime() - 2L);
    Date preStart = new Date(now.getTime() - 3L);
    Date end = new Date(now.getTime() + 2L);
    Date postEnd = new Date(now.getTime() + 3L);
    SpinnerDateModel m = new SpinnerDateModel(now, start, end, 
            Calendar.MILLISECOND);
    m.addChangeListener(this);
    m.setValue(v1);
    harness.check(m.getValue(), v1);
    harness.check(this.event.getSource(), m);
    
    // same value triggers no event
    this.event = null;
    m.setValue(v1);
    harness.check(this.event == null);
    
    // set value less than start
    m.setValue(preStart);        
    harness.check(m.getValue(), preStart);

    // set value greater than maximum
    m.setValue(postEnd);        
    harness.check(m.getValue(), postEnd);
  
    // set null value
    boolean pass = false;
    try
    {
      m.setValue(null);        
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
    // set a non-date value
    pass = false;
    try
    {
      m.setValue("123");        
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
  }
}
