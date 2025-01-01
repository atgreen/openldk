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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.util.Vector;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.Vector;

/**
 * Some tests for the copyInto() method in the {@link Vector} class.
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
    Vector v1 = new Vector();
    v1.add("A");
    v1.add("B");
    v1.add("C");
    Object[] array1 = new Object[3];
    v1.copyInto(array1);
    harness.check(array1[0], "A");
    harness.check(array1[1], "B");
    harness.check(array1[2], "C");

    // array longer than necessary
    Object[] array2 = new Object[] {"1", "2", "3", "4"};
    v1.copyInto(array2);
    harness.check(array2[0], "A");
    harness.check(array2[1], "B");
    harness.check(array2[2], "C");
    harness.check(array2[3], "4");

    // array shorter than necessary
    Object[] array3 = new Object[] {"1", "2"};
    boolean pass = false;
    try
    {
      v1.copyInto(array3);      
    }
    catch (IndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
    harness.check(array3[0], "1");  // the method fails without modifying the
    harness.check(array3[1], "2");  // array
    
    // try null array
    pass = false;
    try
    {
      v1.copyInto(null);      
    }
    catch (NullPointerException e)
    {
      pass = true;
    }
    harness.check(pass);
    
  }

}