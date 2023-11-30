// Tags: JDK1.2
// Uses: MyChangeListener

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */
 
package gnu.testlet.javax.swing.JSlider;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JSlider;

/**
 * Some checks for the addChangeListener() method in the {@link JSlider} class.
 */
public class addChangeListener implements Testlet {

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    JSlider slider = new JSlider();
    MyChangeListener listener = new MyChangeListener();
    slider.addChangeListener(listener);
    slider.setExtent(1);
    harness.check(listener.event.getSource(), slider);
    
    // adding a null listener is permitted
    boolean pass = true;
    try
    {
      slider.addChangeListener(null);
    }
    catch (Exception e)
    {
      pass = false;
    }
    harness.check(pass);
  }

}
