// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

// This file is part of Mauve.

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

package gnu.testlet.javax.swing.DefaultListModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.DefaultListModel;

/**
 * Some tests for the copyInto() method in the {@link DefaultListModel} class.
 */
public class copyInto implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    harness.checkPoint("copyInto(Object[])");
    DefaultListModel m1 = new DefaultListModel();
    m1.addElement("A");
    m1.addElement(null);
    m1.addElement("C");
    Object[] dest = new Object[3];
    m1.copyInto(dest);
    harness.check(dest[0], "A");
    harness.check(dest[1], null);
    harness.check(dest[2], "C");
    
    boolean pass = false;
    dest = new Object[2];
    try
    {
      m1.copyInto(dest);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    pass = false;
    try 
    {
      m1.copyInto(null);
    }
    catch (NullPointerException e) 
    {
      pass = true;
    }
    harness.check(pass);
    
    dest = new Object[4];
    dest[3] = "X";
    m1.copyInto(dest);
    harness.check(dest[3], "X");
  }

}
