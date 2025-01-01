/* setDesignTime.java -- some checks for the setDesignTime() method in
       the BeanContextSupport class.
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

package gnu.testlet.java.beans.beancontext.BeanContextSupport;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.beancontext.BeanContextSupport;

public class setDesignTime implements Testlet, PropertyChangeListener
{
  PropertyChangeEvent lastEvent;
  
  public void propertyChange(PropertyChangeEvent evt) 
  {
    lastEvent = evt;
  }

  /**
   * See bug parade entry 4295174
   */
  public void test(TestHarness harness)
  {
    BeanContextSupport bcs = new BeanContextSupport();
    bcs.addPropertyChangeListener("designTime", this);
    harness.check(bcs.isDesignTime(), false);
    bcs.setDesignTime(true);
    harness.check(lastEvent, null);
    bcs.addPropertyChangeListener("designMode", this);
    bcs.setDesignTime(false);
    harness.check(lastEvent.getPropertyName(), "designMode");
    harness.check(lastEvent.getNewValue(), Boolean.FALSE);
    harness.check(lastEvent.getOldValue(), Boolean.TRUE);
    harness.check(lastEvent.getSource(), bcs);
  }
}
