// Tags: JDK1.2

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.awt.image.ComponentColorModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.*;
import java.util.Arrays;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class createCompatibleSampleModel
  implements Testlet
{
  public void test(TestHarness h)
  {
    test_BYTE(h);
    test_USHORT(h);
    test_SHORT(h);
    test_INT(h);
    test_FLOAT(h);
    test_DOUBLE(h);
  }


  private void test_BYTE(TestHarness h)
  {
    SampleModel sm;

    h.checkPoint("BYTE");
    sm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                 new int[] { 2, 2, 2 },
                                 /* alpha */ false,
                                 /* premultipliedAlpha */ false,
                                 Transparency.OPAQUE,
                                 DataBuffer.TYPE_BYTE)
      .createCompatibleSampleModel(2, 3);

    h.check(sm instanceof PixelInterleavedSampleModel);       // Check #1.
    h.check(sm.getTransferType(), DataBuffer.TYPE_BYTE);      // Check #2.
    h.check(sm.getWidth(), 2);                                // Check #3.
    h.check(sm.getHeight(), 3);                               // Check #4.
    h.check(sm.getNumBands(), 3);                             // Check #5.
    h.check(Arrays.equals(sm.getSampleSize(),                 // Check #6.
                          new int[] { 8, 8, 8 }));
    h.check(((ComponentSampleModel) sm).getOffset(1, 1), 9);  // Check #7.
    h.check(Arrays.equals(((ComponentSampleModel) sm).getBandOffsets(),
                          new int[] { 0, 1, 2 }));            // Check #8.
    h.check(sm.createDataBuffer().getSize(), 18);             // Check #9.
  }


  private void test_USHORT(TestHarness h)
  {
    SampleModel sm;

    h.checkPoint("USHORT");
    sm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                 new int[] { 8, 2, 3, 1 },
                                 /* alpha */ true,
                                 /* premultipliedAlpha */ false,
                                 Transparency.BITMASK,
                                 DataBuffer.TYPE_USHORT)
      .createCompatibleSampleModel(4, 3);

    h.check(sm instanceof PixelInterleavedSampleModel);       // Check #1.
    h.check(sm.getTransferType(), DataBuffer.TYPE_USHORT);    // Check #2.
    h.check(sm.getWidth(), 4);                                // Check #3.
    h.check(sm.getHeight(), 3);                               // Check #4.
    h.check(sm.getNumBands(), 4);                             // Check #5.
    h.check(Arrays.equals(sm.getSampleSize(),                 // Check #6.
                          new int[] { 16, 16, 16, 16 }));
    h.check(((ComponentSampleModel) sm).getOffset(1, 1), 20); // Check #7.
    h.check(Arrays.equals(((ComponentSampleModel) sm).getBandOffsets(),
                          new int[] { 0, 1, 2, 3 }));         // Check #8.
    h.check(sm.createDataBuffer().getSize(), 48);             // Check #9.
  }


  private void test_SHORT(TestHarness h)
  {
    SampleModel sm;

    h.checkPoint("SHORT");
    sm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                 new int[] { 5, 5, 6 },
                                 /* alpha */ false,
                                 /* premultipliedAlpha */ false,
                                 Transparency.OPAQUE,
                                 DataBuffer.TYPE_SHORT)
      .createCompatibleSampleModel(2, 3);

    h.check(sm.getClass(), ComponentSampleModel.class);       // Check #1.
    h.check(sm.getTransferType(), DataBuffer.TYPE_SHORT);     // Check #2.
    h.check(sm.getWidth(), 2);                                // Check #3.
    h.check(sm.getHeight(), 3);                               // Check #4.
    h.check(sm.getNumBands(), 3);                             // Check #5.
    h.check(Arrays.equals(sm.getSampleSize(),                 // Check #6.
                          new int[] { 16, 16, 16 }));
    h.check(((ComponentSampleModel) sm).getOffset(1, 1), 9);  // Check #7.
    h.check(Arrays.equals(((ComponentSampleModel) sm).getBandOffsets(),
                          new int[] { 0, 1, 2 }));            // Check #8.
    h.check(sm.createDataBuffer().getSize(), 18);             // Check #9.
  }


  private void test_INT(TestHarness h)
  {
    SampleModel sm;

    h.checkPoint("INT");
    sm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_GRAY),
                                 new int[] { 24 },
                                 /* alpha */ false,
                                 /* premultipliedAlpha */ false,
                                 Transparency.OPAQUE,
                                 DataBuffer.TYPE_INT)
      .createCompatibleSampleModel(2, 3);

    h.check(sm.getClass(), ComponentSampleModel.class);       // Check #1.
    h.check(sm.getTransferType(), DataBuffer.TYPE_INT);       // Check #2.
    h.check(sm.getWidth(), 2);                                // Check #3.
    h.check(sm.getHeight(), 3);                               // Check #4.
    h.check(sm.getNumBands(), 1);                             // Check #5.
    h.check(Arrays.equals(sm.getSampleSize(),                 // Check #6.
                          new int[] { 32 }));
    h.check(((ComponentSampleModel) sm).getOffset(1, 1), 3);  // Check #7.
    h.check(Arrays.equals(((ComponentSampleModel) sm).getBandOffsets(),
                          new int[] { 0 }));                  // Check #8.
    h.check(sm.createDataBuffer().getSize(), 6);              // Check #9.
  }


  private void test_FLOAT(TestHarness h)
  {
    SampleModel sm;

    h.checkPoint("FLOAT");
    sm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                 new int[] { 32, 32, 32, 32 },
                                 /* alpha */ true,
                                 /* premultipliedAlpha */ false,
                                 Transparency.TRANSLUCENT,
                                 DataBuffer.TYPE_FLOAT)
      .createCompatibleSampleModel(4, 2);

    h.check(sm.getClass(), ComponentSampleModel.class);       // Check #1.
    h.check(sm.getTransferType(), DataBuffer.TYPE_FLOAT);     // Check #2.
    h.check(sm.getWidth(), 4);                                // Check #3.
    h.check(sm.getHeight(), 2);                               // Check #4.
    h.check(sm.getNumBands(), 4);                             // Check #5.
    h.check(Arrays.equals(sm.getSampleSize(),                 // Check #6.
                          new int[] { 32, 32, 32, 32 }));
    h.check(((ComponentSampleModel) sm).getOffset(3, 1), 28); // Check #7.
    h.check(Arrays.equals(((ComponentSampleModel) sm).getBandOffsets(),
                          new int[] { 0, 1, 2, 3 }));         // Check #8.
    h.check(sm.createDataBuffer().getSize(), 32);             // Check #9.
  }


  private void test_DOUBLE(TestHarness h)
  {
    SampleModel sm;

    h.checkPoint("DOUBLE");
    sm = new ComponentColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                 new int[] { 64, 64, 64, 64 },
                                 /* alpha */ true,
                                 /* premultipliedAlpha */ true,
                                 Transparency.TRANSLUCENT,
                                 DataBuffer.TYPE_DOUBLE)
      .createCompatibleSampleModel(2, 2);

    h.check(sm.getClass(), ComponentSampleModel.class);       // Check #1.
    h.check(sm.getTransferType(), DataBuffer.TYPE_DOUBLE);    // Check #2.
    h.check(sm.getWidth(), 2);                                // Check #3.
    h.check(sm.getHeight(), 2);                               // Check #4.
    h.check(sm.getNumBands(), 4);                             // Check #5.
    h.check(Arrays.equals(sm.getSampleSize(),                 // Check #6.
                          new int[] { 64, 64, 64, 64 }));
    h.check(((ComponentSampleModel) sm).getOffset(1, 1), 12); // Check #7.
    h.check(Arrays.equals(((ComponentSampleModel) sm).getBandOffsets(),
                          new int[] { 0, 1, 2, 3 }));         // Check #8.
    h.check(sm.createDataBuffer().getSize(), 16);             // Check #9.
  }
}
