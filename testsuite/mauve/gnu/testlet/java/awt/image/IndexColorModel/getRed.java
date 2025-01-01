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

package gnu.testlet.java.awt.image.IndexColorModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.IndexColorModel;

/**
 * Some checks for the getRed() method.
 */
public class getRed implements Testlet 
{

  private static final byte[] R4 = {(byte) 1, (byte) 2, (byte) 3, (byte) 4};
  private static final byte[] G4 = {(byte) 5, (byte) 6, (byte) 7, (byte) 8};
  private static final byte[] B4 = {(byte) 9, (byte) 10, (byte) 11, (byte) 12};

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    IndexColorModel m1 = new IndexColorModel(2, 4, R4, G4, B4);
    harness.check(m1.getRed(0), 1);
    harness.check(m1.getRed(1), 2);
    harness.check(m1.getRed(2), 3);
    harness.check(m1.getRed(3), 4);
 
    // try negative pixel
    boolean pass = false;
    try
    {
      m1.getRed(-1);   
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      pass = true;
    }
    harness.check(pass);
 
    // try pixel too large
    harness.check(m1.getRed(4), 0);
}  

}
