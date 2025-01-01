// Tags: JDK1.2

// Copyright (C) 2004 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.image.ShortLookupTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.ShortLookupTable;

/**
 * Some checks for the lookupPixel() methods in the {@link ShortLookupTable} class.
 */
public class lookupPixel implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testInt(harness);
    testShort(harness);
    testFailure(harness);
  }

  private void testInt(TestHarness harness) 
  {
    harness.checkPoint("(int[], int[])");
    short[] data = {105, 104, 103, 102, 101, 100};
    ShortLookupTable t = new ShortLookupTable(100, data);
    
    // check 1-band source with 1-band lookup table
    int[] src = new int[] {100};
    int[] dst = t.lookupPixel(src, null);
    harness.check(dst[0], 105);
    
    src = new int[] {101};
    dst = new int[] {0};
    dst = t.lookupPixel(src, dst);
    harness.check(dst[0], 104);
    
    // check 3-band source with 1-band lookup table
    src = new int[] {100, 101, 102};
    try {
        dst = t.lookupPixel(src, null);
        harness.check(dst[0], 105);
        harness.check(dst[1], 104);
        harness.check(dst[2], 103);
    }
    catch (Exception e) {      // don't allow a failure to 
        harness.check(false);  // disrupt remaining checks
        harness.debug(e);
    }

    src = new int[] {102, 103, 104};
    dst = new int[] {0, 0, 0};
    try {
        dst = t.lookupPixel(src, dst);
        harness.check(dst[0], 103);
        harness.check(dst[1], 102);
        harness.check(dst[2], 101);
    }
    catch (Exception e) {      // don't allow a failure to 
        harness.check(false);  // disrupt remaining checks
        harness.debug(e);
    }

    // check 3-band source with 3-band lookup table
    short[][] data2 = {
      {105, 104, 103, 102, 101, 100},
      {115, 114, 113, 112, 111, 110},
      {125, 124, 123, 122, 121, 120}
    };
    ShortLookupTable t2 = new ShortLookupTable(100, data2);
    
    int[] src2 = {100, 101, 102};
    dst = t2.lookupPixel(src2, null);
    harness.check(dst[0], 105);
    harness.check(dst[1], 114);
    harness.check(dst[2], 123);
    
    src2 = new int[] {103, 104, 105};
    dst = new int[] {0, 0, 0};
    dst = t2.lookupPixel(src2, dst);
    harness.check(dst[0], 102);
    harness.check(dst[1], 111);
    harness.check(dst[2], 120);

    // check 1-band source with 2-band lookup table
    short[][] data3 = {
      {105, 104, 103, 102, 101, 100},
      {115, 114, 113, 112, 111, 110},
    };
    ShortLookupTable t3 = new ShortLookupTable(100, data3);
    src = new int[] {100};
    dst = t3.lookupPixel(src, null);
    harness.check(dst[0], 105);

    src = new int[] {101};
    dst = new int[] {0};
    dst = t3.lookupPixel(src, dst);
    harness.check(dst[0], 104);

    // check 3-band source with 2-band lookup table
    try {
        dst = t3.lookupPixel(src2, null);
        harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e) {
        harness.check(true);
    }

    dst = new int[3];
    try {
        dst = t3.lookupPixel(src2, dst);
        harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e) {
        harness.check(true);
    }
    
  }

  private void testShort(TestHarness harness)   
  {
    harness.checkPoint("(short[], short[])");
    short[] data = {105, 104, 103, 102, 101, 100};
    ShortLookupTable t = new ShortLookupTable(100, data);
    
    // check 1-band source with 1-band lookup table
    short[] src = new short[] {100};
    short[] dst = t.lookupPixel(src, null);
    harness.check(dst[0], 105);
    
    src = new short[] {101};
    dst = new short[] {0};
    dst = t.lookupPixel(src, dst);
    harness.check(dst[0], 104);

    // check 3-band source with 1-band lookup table
    src = new short[] {100, 101, 102};
    try {
        dst = t.lookupPixel(src, null);
        harness.check(dst[0], 105);
        harness.check(dst[1], 104);
        harness.check(dst[2], 103);
    }
    catch (Exception e) {      // don't allow a failure to 
        harness.check(false);  // disrupt remaining checks
        harness.debug(e);
    }
    
    src = new short[] {102, 103, 104};
    dst = new short[] {0, 0, 0};
    try {
        dst = t.lookupPixel(src, dst);
        harness.check(dst[0], 103);
        harness.check(dst[1], 102);
        harness.check(dst[2], 101);
    }
    catch (Exception e) {      // don't allow a failure to 
        harness.check(false);  // disrupt remaining checks
        harness.debug(e);
    }

    // check 3-band source with 3-band lookup table
    short[][] data2 = {
      {105, 104, 103, 102, 101, 100},
      {115, 114, 113, 112, 111, 110},
      {125, 124, 123, 122, 121, 120}
    };
    ShortLookupTable t2 = new ShortLookupTable(100, data2);
    short[] src2 = {100, 101, 102};
    dst = t2.lookupPixel(src2, null);
    harness.check(dst[0], 105);
    harness.check(dst[1], 114);
    harness.check(dst[2], 123);
    
    src2 = new short[] {103, 104, 105};
    dst = new short[] {0, 0, 0};
    dst = t2.lookupPixel(src2, dst);
    harness.check(dst[0], 102);
    harness.check(dst[1], 111);
    harness.check(dst[2], 120);

    // check 1-band source with 2-band lookup table
    short[][] data3 = {
      {105, 104, 103, 102, 101, 100},
      {115, 114, 113, 112, 111, 110},
    };
    ShortLookupTable t3 = new ShortLookupTable(100, data3);
    src = new short[] {100};
    dst = t3.lookupPixel(src, null);
    harness.check(dst[0], 105);
    
    src = new short[] {101};
    dst = new short[1];
    dst = t3.lookupPixel(src, dst);
    harness.check(dst[0], 104);

    // check 3 band source, 2 band lookup table
    try {
        dst = t3.lookupPixel(src2, null);
        harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e) {
        harness.check(true);
    }
   
    dst = new short[3];
    try {
        dst = t3.lookupPixel(src2, dst);
        harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e) {
        harness.check(true);
    }
  }

  // Test failures, ie, if the requested pixel is not in the table.
  // It should throw array index out of bounds exceptions.
  private void testFailure(TestHarness harness)
  {
    harness.checkPoint("not in table");
    short[] data = {105, 104, 103, 102, 101, 100};
    ShortLookupTable t = new ShortLookupTable(100, data);
    
    try
    {
      int[] src = new int[] {120};
      t.lookupPixel(src, null);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException ex)
    {
      harness.check(true);
    }
    
    try
    {
      short[] src = new short[] {120};
      t.lookupPixel(src, null);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException ex)
    {
      harness.check(true);
    }
    
  }
}

