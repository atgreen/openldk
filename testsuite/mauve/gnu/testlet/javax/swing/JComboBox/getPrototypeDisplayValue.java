// Tags: JDK1.4

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
// Boston, MA 02111-1307, USA.  

package gnu.testlet.javax.swing.JComboBox;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JComboBox;
import javax.swing.JComponent;

/**
 * Some checks for the getPrototypeDisplayValue() method in the 
 * {@link JComponent} class.
 */
public class getPrototypeDisplayValue 
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    JComboBox c = new JComboBox(new Object[] {"A", "B", "C"});
    
    // check default value
    harness.check(c.getPrototypeDisplayValue(), null);
    
    // set a new value and retrieve it
    c.setPrototypeDisplayValue("XYZ");
    harness.check(c.getPrototypeDisplayValue(), "XYZ");
    
    // try a null value
    c.setPrototypeDisplayValue(null);
    harness.check(c.getPrototypeDisplayValue(), null);
  }
  
}

