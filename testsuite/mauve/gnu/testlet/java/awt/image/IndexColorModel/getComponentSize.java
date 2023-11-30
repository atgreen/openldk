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
 * Some checks for the getComponentSize() method.
 */
public class getComponentSize implements Testlet 
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
    int[] cs1 = m1.getComponentSize();
    harness.check(cs1.length, 3);
    harness.check(cs1[0], 8);
    harness.check(cs1[1], 8);
    harness.check(cs1[2], 8);

    IndexColorModel m2 = new IndexColorModel(2, 4, R4, G4, B4, 2);
    int[] cs2 = m2.getComponentSize();
    harness.check(cs2.length, 4);
    harness.check(cs2[0], 8);
    harness.check(cs2[1], 8);
    harness.check(cs2[2], 8);
    harness.check(cs2[3], 8);
 
    IndexColorModel m3a = new IndexColorModel(2, 4, CMAP, 0, false);
    int[] cs3a = m3a.getComponentSize();
    harness.check(cs3a.length, 3);
    harness.check(cs3a[0], 8);
    harness.check(cs3a[1], 8);
    harness.check(cs3a[2], 8);

    IndexColorModel m3b = new IndexColorModel(2, 4, CMAP, 0, false, 1);
    int[] cs3b = m3b.getComponentSize();
    harness.check(cs3b.length, 4);
    harness.check(cs3b[0], 8);
    harness.check(cs3b[1], 8);
    harness.check(cs3b[2], 8);
    harness.check(cs3b[3], 8);

    IndexColorModel m4 = new IndexColorModel(2, 4, CMAP_WITH_ALPHA, 0, true);
    int[] cs4 = m4.getComponentSize();
    harness.check(cs4.length, 4);
    harness.check(cs4[0], 8);
    harness.check(cs4[1], 8);
    harness.check(cs4[2], 8);
    harness.check(cs4[3], 8);

    IndexColorModel m5 = new IndexColorModel(2, 4, CMAP_WITH_ALPHA0, 0, true);
    int[] cs5 = m5.getComponentSize();
    harness.check(cs5.length, 4);
    harness.check(cs5[0], 8);
    harness.check(cs5[1], 8);
    harness.check(cs5[2], 8);
    harness.check(cs5[3], 8);

    IndexColorModel m6 = new IndexColorModel(2, 4, CMAP_WITH_ALPHA1, 0, true);
    int[] cs6 = m6.getComponentSize();
    harness.check(cs6.length, 3);
    harness.check(cs6[0], 8);
    harness.check(cs6[1], 8);
    harness.check(cs6[2], 8);
  }  

}