/* setOrientation.java -- some checks for the setOrientation()
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JProgressBar;
import javax.swing.SwingConstants;

public class setOrientation implements Testlet, PropertyChangeListener
{
  PropertyChangeEvent lastEvent;
  
  public void propertyChange(PropertyChangeEvent event) 
  {
    lastEvent = event;
  }

  public void test(TestHarness harness)
  {
    JProgressBar pb = new JProgressBar();
    harness.check(pb.getOrientation(), SwingConstants.HORIZONTAL);
    
    pb.addPropertyChangeListener(this);
    pb.setOrientation(SwingConstants.VERTICAL);
    harness.check(pb.getOrientation(), SwingConstants.VERTICAL);
    harness.check(lastEvent.getPropertyName(), "orientation");
    harness.check(lastEvent.getOldValue(), 
            new Integer(SwingConstants.HORIZONTAL));
    harness.check(lastEvent.getNewValue(),             
            new Integer(SwingConstants.VERTICAL));
    
    boolean pass = false;
    try
    {
      pb.setOrientation(99);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }
}
