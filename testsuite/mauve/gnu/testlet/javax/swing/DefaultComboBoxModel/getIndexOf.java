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

package gnu.testlet.javax.swing.DefaultComboBoxModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.DefaultComboBoxModel;

/**
 * Some checks for the getIndexOf() method in the 
 * {@link DefaultComboBoxModel} class.
 */
public class getIndexOf 
  implements Testlet
{
  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {   
    DefaultComboBoxModel m = new DefaultComboBoxModel(new Object[] {"A", "B", "C"});
    harness.check(m.getIndexOf("A"), 0);
    harness.check(m.getIndexOf("B"), 1);
    harness.check(m.getIndexOf("C"), 2);
    harness.check(m.getIndexOf("D"), -1);
    harness.check(m.getIndexOf(null), -1);
    
    m.addElement(null);
    harness.check(m.getIndexOf(null), 3);
  }
}

