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

import java.awt.Color;
import java.awt.image.DataBuffer;
import java.awt.image.IndexColorModel;
import java.math.BigInteger;

/**
 * Some checks for the isValid() methods.
 */
public class isValid implements Testlet 
{

  private static final byte[] R4 = {(byte) 1, (byte) 2, (byte) 3, (byte) 4};
  private static final byte[] G4 = {(byte) 5, (byte) 6, (byte) 7, (byte) 8};
  private static final byte[] B4 = {(byte) 9, (byte) 10, (byte) 11, (byte) 12};

  private static final int[] CMAP_INT = {new Color(1, 5, 9).getRGB(), 
        new Color(2, 6, 10).getRGB(), new Color(3, 7, 11).getRGB(),
        new Color(4, 8, 12).getRGB()};

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
    harness.checkPoint("isValid()");
    IndexColorModel m1 = new IndexColorModel(2, 4, R4, G4, B4);
    harness.check(m1.isValid());

    IndexColorModel m2 = new IndexColorModel(2, 4, CMAP_INT, 0, 
            DataBuffer.TYPE_BYTE, new BigInteger("11"));
    harness.check(!m2.isValid());
  }  

  public void test2(TestHarness harness) 
  {
    harness.checkPoint("isValid(int)");
    IndexColorModel m1 = new IndexColorModel(2, 4, R4, G4, B4);
    harness.check(m1.isValid(0));
    try {
      harness.check(!m1.isValid(-1));
    }
    catch (Exception e)
    {
      harness.debug(e);   
    }
    harness.check(!m1.isValid(4));

    IndexColorModel m2 = new IndexColorModel(2, 4, CMAP_INT, 0, 
            DataBuffer.TYPE_BYTE, new BigInteger("11"));
    harness.check(m2.isValid(0));
    harness.check(m2.isValid(1));
    harness.check(!m2.isValid(2));
    harness.check(m2.isValid(3));
  }  

}
