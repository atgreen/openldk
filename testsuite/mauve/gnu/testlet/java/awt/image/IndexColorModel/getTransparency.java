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

import java.awt.Transparency;
import java.awt.image.IndexColorModel;

/**
 * Some checks for the getTransparency() method.
*/
public class getTransparency implements Testlet 
{

  private static final byte[] R4 = {(byte) 1, (byte) 2, (byte) 3, (byte) 4};
  private static final byte[] G4 = {(byte) 5, (byte) 6, (byte) 7, (byte) 8};
  private static final byte[] B4 = {(byte) 9, (byte) 10, (byte) 11, (byte) 12};

  private static final byte[] CMAP = {(byte) 1, (byte) 5, (byte) 9, (byte) 2, 
        (byte) 6, (byte) 10, (byte) 3, (byte) 7, (byte) 11, (byte) 4, 
        (byte) 8, (byte) 12};

  private static final byte[] CMAP_WITH_ALPHA = {(byte) 1, (byte) 5, (byte) 9, 
        (byte) 13, (byte) 2, (byte) 6, (byte) 10, (byte) 14, (byte) 3, 
        (byte) 7, (byte) 11, (byte) 15, (byte) 4, (byte) 8, (byte) 12, 
        (byte) 16};

  private static final byte[] CMAP_WITH_ALPHA0 = {(byte) 1, (byte) 5, (byte) 9, 
        (byte) 0, (byte) 2, (byte) 6, (byte) 10, (byte) 0, (byte) 3, 
        (byte) 7, (byte) 11, (byte) 0, (byte) 4, (byte) 8, (byte) 12, 
        (byte) 0};

  private static final byte[] CMAP_WITH_ALPHA1 = {(byte) 1, (byte) 5, (byte) 9, 
        (byte) 255, (byte) 2, (byte) 6, (byte) 10, (byte) 255, (byte) 3, 
        (byte) 7, (byte) 11, (byte) 255, (byte) 4, (byte) 8, (byte) 12, 
        (byte) 255};
  
  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness) 
  {
    IndexColorModel m1 = new IndexColorModel(2, 4, R4, G4, B4);
    harness.check(m1.getTransparency(), Transparency.OPAQUE);
  
    IndexColorModel m2a = new IndexColorModel(2, 4, R4, G4, B4, 2);
    harness.check(m2a.getTransparency(), Transparency.BITMASK);
    IndexColorModel m2b = new IndexColorModel(2, 4, R4, G4, B4, -1);
    harness.check(m2b.getTransparency(), Transparency.OPAQUE);
    IndexColorModel m2c = new IndexColorModel(2, 4, R4, G4, B4, 99);
    harness.check(m2c.getTransparency(), Transparency.OPAQUE);
    
    IndexColorModel m3a = new IndexColorModel(2, 4, CMAP, 0, false);
    harness.check(m3a.getTransparency(), Transparency.OPAQUE);
    IndexColorModel m3b = new IndexColorModel(2, 4, CMAP, 0, false, 2);
    harness.check(m3b.getTransparency(), Transparency.BITMASK);

    IndexColorModel m4a = new IndexColorModel(2, 4, CMAP_WITH_ALPHA, 0, true);
    harness.check(m4a.getTransparency(), Transparency.TRANSLUCENT);
    IndexColorModel m4b = new IndexColorModel(2, 4, CMAP_WITH_ALPHA, 0, true, 2);
    harness.check(m4b.getTransparency(), Transparency.TRANSLUCENT);

    IndexColorModel m5a = new IndexColorModel(2, 4, CMAP_WITH_ALPHA0, 0, true);
    harness.check(m5a.getTransparency(), Transparency.BITMASK);
    IndexColorModel m5b = new IndexColorModel(2, 4, CMAP_WITH_ALPHA0, 0, true, 2);
    harness.check(m5b.getTransparency(), Transparency.BITMASK);

    IndexColorModel m6a = new IndexColorModel(2, 4, CMAP_WITH_ALPHA1, 0, true);
    harness.check(m6a.getTransparency(), Transparency.OPAQUE);
    IndexColorModel m6b = new IndexColorModel(2, 4, CMAP_WITH_ALPHA1, 0, true, 2);
    harness.check(m6b.getTransparency(), Transparency.BITMASK);
  }  

}
