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

import java.util.Vector;

import javax.swing.DefaultComboBoxModel;

/**
 * Some checks for the constructors in the {@link DefaultComboBoxModel} class.
 */
public class constructors 
  implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    test1(harness);
    test2(harness);
    test3(harness);
  }
  
  public void test1(TestHarness harness)      
  {   
    harness.checkPoint("()");
    DefaultComboBoxModel m = new DefaultComboBoxModel();
    harness.check(m.getSelectedItem(), null);
    harness.check(m.getSize(), 0);
  }
  
  public void test2(TestHarness harness)      
  {   
    harness.checkPoint("(Object[])");
    DefaultComboBoxModel m = new DefaultComboBoxModel(new Object[] {"A", "B"});
    harness.check(m.getSize(), 2);
    harness.check(m.getSelectedItem(), "A");
    harness.check(m.getElementAt(0), "A");
    harness.check(m.getElementAt(1), "B");
    
    // try null
    boolean pass = false;
    try
    {
      m = new DefaultComboBoxModel((Object[]) null);
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);  
  }

  public void test3(TestHarness harness)      
  {   
    harness.checkPoint("(Vector)");
    Vector v = new Vector();
    v.add("A");
    v.add("B");
    DefaultComboBoxModel m = new DefaultComboBoxModel(v);
    harness.check(m.getSize(), 2);
    harness.check(m.getSelectedItem(), "A");
    harness.check(m.getElementAt(0), "A");
    harness.check(m.getElementAt(1), "B");
    
    // try null
    boolean pass = false;
    try
    {
      m = new DefaultComboBoxModel((Vector) null);    
    }
    catch (NullPointerException e)
    {
      pass = true; 
    }
    harness.check(pass);
  }
}

