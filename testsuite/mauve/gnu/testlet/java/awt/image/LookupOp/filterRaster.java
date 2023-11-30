/* filterRaster.java -- some checks for the filter() methods in the
                        LookupOp class.
   Copyright (C) 2006 Francis Kung <fkung@redhat.com>
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4

package gnu.testlet.java.awt.image.LookupOp;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.ByteLookupTable;
import java.awt.image.DataBuffer;
import java.awt.image.LookupOp;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

public class filterRaster implements Testlet
{
  public void test(TestHarness harness)
  {
    // These tests crash sun's JVM!
    
    //testRaster1(harness);
    //testRaster2(harness);
    //testRaster3(harness);
  }
  
  public void testRaster1(TestHarness harness)
  {
    harness.checkPoint("testRaster1()");
    Raster src = createRasterA();
    WritableRaster dest = src.createCompatibleWritableRaster();
    
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    
    dest = op.filter(src, dest);
    harness.check(dest.getSample(0, 0, 0), 1);
    harness.check(dest.getSample(1, 0, 0), 2);
    harness.check(dest.getSample(2, 0, 0), 3);
    harness.check(dest.getSample(3, 0, 0), 4);
    harness.check(dest.getSample(4, 0, 0), 5);
    harness.check(dest.getSample(0, 1, 0), 6);
    harness.check(dest.getSample(1, 1, 0), 7);
    harness.check(dest.getSample(2, 1, 0), 8);
    harness.check(dest.getSample(3, 1, 0), 9);
    harness.check(dest.getSample(4, 1, 0), 10);
    harness.check(dest.getSample(0, 2, 0), 11);
    harness.check(dest.getSample(1, 2, 0), 12);
    harness.check(dest.getSample(2, 2, 0), 13);
    harness.check(dest.getSample(3, 2, 0), 14);
    harness.check(dest.getSample(4, 2, 0), 15);
    harness.check(dest.getSample(0, 3, 0), 16);
    harness.check(dest.getSample(1, 3, 0), 17);
    harness.check(dest.getSample(2, 3, 0), 18);
    harness.check(dest.getSample(3, 3, 0), 19);
    harness.check(dest.getSample(4, 3, 0), 20);

    harness.check(dest.getSample(0, 0, 1), 11);
    harness.check(dest.getSample(1, 0, 1), 12);
    harness.check(dest.getSample(2, 0, 1), 13);
    harness.check(dest.getSample(3, 0, 1), 14);
    harness.check(dest.getSample(4, 0, 1), 15);
    harness.check(dest.getSample(0, 1, 1), 16);
    harness.check(dest.getSample(1, 1, 1), 17);
    harness.check(dest.getSample(2, 1, 1), 18);
    harness.check(dest.getSample(3, 1, 1), 19);
    harness.check(dest.getSample(4, 1, 1), 20);
    harness.check(dest.getSample(0, 2, 1), 21);
    harness.check(dest.getSample(1, 2, 1), 22);
    harness.check(dest.getSample(2, 2, 1), 23);
    harness.check(dest.getSample(3, 2, 1), 24);
    harness.check(dest.getSample(4, 2, 1), 25);
    harness.check(dest.getSample(0, 3, 1), 26);
    harness.check(dest.getSample(1, 3, 1), 27);
    harness.check(dest.getSample(2, 3, 1), 28);
    harness.check(dest.getSample(3, 3, 1), 29);
    harness.check(dest.getSample(4, 3, 1), 30);

    harness.check(dest.getSample(0, 0, 2), 21);
    harness.check(dest.getSample(1, 0, 2), 22);
    harness.check(dest.getSample(2, 0, 2), 23);
    harness.check(dest.getSample(3, 0, 2), 24);
    harness.check(dest.getSample(4, 0, 2), 25);
    harness.check(dest.getSample(0, 1, 2), 26);
    harness.check(dest.getSample(1, 1, 2), 27);
    harness.check(dest.getSample(2, 1, 2), 28);
    harness.check(dest.getSample(3, 1, 2), 29);
    harness.check(dest.getSample(4, 1, 2), 30);
    harness.check(dest.getSample(0, 2, 2), 31);
    harness.check(dest.getSample(1, 2, 2), 32);
    harness.check(dest.getSample(2, 2, 2), 33);
    harness.check(dest.getSample(3, 2, 2), 34);
    harness.check(dest.getSample(4, 2, 2), 35);
    harness.check(dest.getSample(0, 3, 2), 36);
    harness.check(dest.getSample(1, 3, 2), 37);
    harness.check(dest.getSample(2, 3, 2), 38);
    harness.check(dest.getSample(3, 3, 2), 39);
    harness.check(dest.getSample(4, 3, 2), 40);
  }
    
  public void testRaster2(TestHarness harness)
  {
    harness.checkPoint("testRaster2()");
    Raster src = createRasterA();
    WritableRaster dest = src.createCompatibleWritableRaster();
    
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    dest = op.filter(src, dest);
    
    harness.check(dest.getSample(0, 0, 0), 0);
    harness.check(dest.getSample(1, 0, 0), 0);
    harness.check(dest.getSample(2, 0, 0), 0);
    harness.check(dest.getSample(3, 0, 0), 0);
    harness.check(dest.getSample(4, 0, 0), 0);
    harness.check(dest.getSample(0, 1, 0), 0);
    harness.check(dest.getSample(1, 1, 0), 7);
    harness.check(dest.getSample(2, 1, 0), 8);
    harness.check(dest.getSample(3, 1, 0), 9);
    harness.check(dest.getSample(4, 1, 0), 0);
    harness.check(dest.getSample(0, 2, 0), 0);
    harness.check(dest.getSample(1, 2, 0), 12);
    harness.check(dest.getSample(2, 2, 0), 13);
    harness.check(dest.getSample(3, 2, 0), 14);
    harness.check(dest.getSample(4, 2, 0), 0);
    harness.check(dest.getSample(0, 3, 0), 0);
    harness.check(dest.getSample(1, 3, 0), 0);
    harness.check(dest.getSample(2, 3, 0), 0);
    harness.check(dest.getSample(3, 3, 0), 0);
    harness.check(dest.getSample(4, 3, 0), 0);

    harness.check(dest.getSample(0, 0, 1), 0);
    harness.check(dest.getSample(1, 0, 1), 0);
    harness.check(dest.getSample(2, 0, 1), 0);
    harness.check(dest.getSample(3, 0, 1), 0);
    harness.check(dest.getSample(4, 0, 1), 0);
    harness.check(dest.getSample(0, 1, 1), 0);
    harness.check(dest.getSample(1, 1, 1), 17);
    harness.check(dest.getSample(2, 1, 1), 18);
    harness.check(dest.getSample(3, 1, 1), 19);
    harness.check(dest.getSample(4, 1, 1), 0);
    harness.check(dest.getSample(0, 2, 1), 0);
    harness.check(dest.getSample(1, 2, 1), 22);
    harness.check(dest.getSample(2, 2, 1), 23);
    harness.check(dest.getSample(3, 2, 1), 24);
    harness.check(dest.getSample(4, 2, 1), 0);
    harness.check(dest.getSample(0, 3, 1), 0);
    harness.check(dest.getSample(1, 3, 1), 0);
    harness.check(dest.getSample(2, 3, 1), 0);
    harness.check(dest.getSample(3, 3, 1), 0);
    harness.check(dest.getSample(4, 3, 1), 0);

    harness.check(dest.getSample(0, 0, 2), 0);
    harness.check(dest.getSample(1, 0, 2), 0);
    harness.check(dest.getSample(2, 0, 2), 0);
    harness.check(dest.getSample(3, 0, 2), 0);
    harness.check(dest.getSample(4, 0, 2), 0);
    harness.check(dest.getSample(0, 1, 2), 0);
    harness.check(dest.getSample(1, 1, 2), 27);
    harness.check(dest.getSample(2, 1, 2), 28);
    harness.check(dest.getSample(3, 1, 2), 29);
    harness.check(dest.getSample(4, 1, 2), 0);
    harness.check(dest.getSample(0, 2, 2), 0);
    harness.check(dest.getSample(1, 2, 2), 32);
    harness.check(dest.getSample(2, 2, 2), 33);
    harness.check(dest.getSample(3, 2, 2), 34);
    harness.check(dest.getSample(4, 2, 2), 0);
    harness.check(dest.getSample(0, 3, 2), 0);
    harness.check(dest.getSample(1, 3, 2), 0);
    harness.check(dest.getSample(2, 3, 2), 0);
    harness.check(dest.getSample(3, 3, 2), 0);
    harness.check(dest.getSample(4, 3, 2), 0);
  }
  
  public void testRaster3(TestHarness harness)
  {
    harness.checkPoint("testRaster3()");
    Raster src = createRasterA();
    WritableRaster dest = src.createCompatibleWritableRaster();
    
    byte[] bytes = new byte[] {(byte) 0xAA, (byte) 0xBB};
    ByteLookupTable t = new ByteLookupTable(0, bytes);
    LookupOp op = new LookupOp(t, null);
    
    dest = op.filter(src, dest);
    harness.check(dest.getSample(0, 0, 0), 1);
    harness.check(dest.getSample(1, 0, 0), 2);
    harness.check(dest.getSample(2, 0, 0), 3);
    harness.check(dest.getSample(3, 0, 0), 4);
    harness.check(dest.getSample(4, 0, 0), 5);
    harness.check(dest.getSample(0, 1, 0), 6);
    harness.check(dest.getSample(1, 1, 0), 21);
    harness.check(dest.getSample(2, 1, 0), 26);
    harness.check(dest.getSample(3, 1, 0), 30);
    harness.check(dest.getSample(4, 1, 0), 10);
    harness.check(dest.getSample(0, 2, 0), 11);
    harness.check(dest.getSample(1, 2, 0), 44);
    harness.check(dest.getSample(2, 2, 0), 48);
    harness.check(dest.getSample(3, 2, 0), 53);
    harness.check(dest.getSample(4, 2, 0), 15);
    harness.check(dest.getSample(0, 3, 0), 16);
    harness.check(dest.getSample(1, 3, 0), 17);
    harness.check(dest.getSample(2, 3, 0), 18);
    harness.check(dest.getSample(3, 3, 0), 19);
    harness.check(dest.getSample(4, 3, 0), 20);

    harness.check(dest.getSample(0, 0, 1), 11);
    harness.check(dest.getSample(1, 0, 1), 12);
    harness.check(dest.getSample(2, 0, 1), 13);
    harness.check(dest.getSample(3, 0, 1), 14);
    harness.check(dest.getSample(4, 0, 1), 15);
    harness.check(dest.getSample(0, 1, 1), 16);
    harness.check(dest.getSample(1, 1, 1), 66);
    harness.check(dest.getSample(2, 1, 1), 71);
    harness.check(dest.getSample(3, 1, 1), 75);
    harness.check(dest.getSample(4, 1, 1), 20);
    harness.check(dest.getSample(0, 2, 1), 21);
    harness.check(dest.getSample(1, 2, 1), 89);
    harness.check(dest.getSample(2, 2, 1), 93);
    harness.check(dest.getSample(3, 2, 1), 98);
    harness.check(dest.getSample(4, 2, 1), 25);
    harness.check(dest.getSample(0, 3, 1), 26);
    harness.check(dest.getSample(1, 3, 1), 27);
    harness.check(dest.getSample(2, 3, 1), 28);
    harness.check(dest.getSample(3, 3, 1), 29);
    harness.check(dest.getSample(4, 3, 1), 30);

    harness.check(dest.getSample(0, 0, 2), 21);
    harness.check(dest.getSample(1, 0, 2), 22);
    harness.check(dest.getSample(2, 0, 2), 23);
    harness.check(dest.getSample(3, 0, 2), 24);
    harness.check(dest.getSample(4, 0, 2), 25);
    harness.check(dest.getSample(0, 1, 2), 26);
    harness.check(dest.getSample(1, 1, 2), 111);
    harness.check(dest.getSample(2, 1, 2), 116);
    harness.check(dest.getSample(3, 1, 2), 120);
    harness.check(dest.getSample(4, 1, 2), 30);
    harness.check(dest.getSample(0, 2, 2), 31);
    harness.check(dest.getSample(1, 2, 2), 134);
    harness.check(dest.getSample(2, 2, 2), 138);
    harness.check(dest.getSample(3, 2, 2), 143);
    harness.check(dest.getSample(4, 2, 2), 35);
    harness.check(dest.getSample(0, 3, 2), 36);
    harness.check(dest.getSample(1, 3, 2), 37);
    harness.check(dest.getSample(2, 3, 2), 38);
    harness.check(dest.getSample(3, 3, 2), 39);

    harness.check(dest.getSample(4, 3, 2), 40);
  }

  /**
   * Creates a sample raster for testing.
   * 
   * @return A raster.
   */
  private Raster createRasterA()
  {
    WritableRaster r = Raster.createInterleavedRaster(DataBuffer.TYPE_BYTE, 5, 
            4, 3, null);
    for (int i = 0; i < 5; i++)
      {
        for (int j = 0; j < 4; j++)
          {
            r.setSample(i, j, 0, j * 5 + i + 1);
            r.setSample(i, j, 1, j * 5 + i + 11);
            r.setSample(i, j, 2, j * 5 + i + 21);
          }
      }
    return r;
  }
}
