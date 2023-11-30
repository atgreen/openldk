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
 * Some tests for the lastIndexOf() method in the {@link DefaultListModel} class.
 */
public class lastIndexOf implements Testlet 
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
  }
  
  public void test1(TestHarness harness) 
  {
    harness.checkPoint("lastIndexOf(Object)");
    DefaultListModel m1 = new DefaultListModel();
    m1.addElement("A");
    m1.addElement("B");
    m1.addElement("C");
    m1.addElement(null);
    m1.addElement("C");
    m1.addElement("B");
    m1.addElement("A");
    harness.check(m1.lastIndexOf("A"), 6);
    harness.check(m1.lastIndexOf("B"), 5);
    harness.check(m1.lastIndexOf("C"), 4);
    harness.check(m1.lastIndexOf(null), 3);
    harness.check(m1.lastIndexOf("Z"), -1);
  }

  public void test2(TestHarness harness) 
  {
    harness.checkPoint("lastIndexOf(Object, int)");
    DefaultListModel m1 = new DefaultListModel();
    m1.addElement("A");
    m1.addElement("B");
    m1.addElement("C");
    m1.addElement(null);
    m1.addElement("C");
    m1.addElement("B");
    m1.addElement("A");
    harness.check(m1.lastIndexOf("A", 6), 6);
    harness.check(m1.lastIndexOf("A", 5), 0);
    harness.check(m1.lastIndexOf("B", 6), 5);
    harness.check(m1.lastIndexOf("B", 5), 5);
    harness.check(m1.lastIndexOf("B", 4), 1);
    harness.check(m1.lastIndexOf(null, 4), 3);
    harness.check(m1.lastIndexOf(null, 3), 3);
    harness.check(m1.lastIndexOf(null, 2), -1);
    harness.check(m1.lastIndexOf("Z", 0), -1);
    harness.check(m1.lastIndexOf("Z", -1), -1);

    boolean pass = false;
    try
    {
      int i = m1.lastIndexOf("Z", 99);
    }
    catch (IndexOutOfBoundsException e) 
    {
      pass = true;    
    }
    harness.check(pass);
  }

}
