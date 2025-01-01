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

package gnu.testlet.java.awt.image.PixelInterleavedSampleModel;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.image.DataBuffer;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.SampleModel;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class createSubsetSampleModel
  implements Testlet
{
  public void test(TestHarness h)
  {
    SampleModel sm;
    PixelInterleavedSampleModel psm, sub;
    int[] subBands;

    psm = new PixelInterleavedSampleModel(DataBuffer.TYPE_BYTE,
                                          5, 2, 4, 20,
                                          new int[] { 3, 0, 1 });
    sm = psm.createSubsetSampleModel(new int[] { 0, 2 });

    // Check #1.
    if (sm instanceof PixelInterleavedSampleModel)
      {
        h.check(true);
        sub = (PixelInterleavedSampleModel) sm;
      }
    else
      {
        h.check(false);
        h.debug(String.valueOf(sm));
        return;
      }

    // Check #2.
    h.check(sub.getDataType(), DataBuffer.TYPE_BYTE);

    // Check #3.
    h.check(sub.getWidth(), 5);

    // Check #4.
    h.check(sub.getHeight(), 2);

    // Check #5.
    h.check(sub.getPixelStride(), 4);

    // Check #6.
    h.check(sub.getScanlineStride(), 20);

    // Check #7.
    subBands = sub.getBandOffsets();
    h.check(subBands.length, 2);

    // Check #8.
    h.check(subBands[0], 3);

    // Check #9.
    h.check(subBands[1], 1);
  }
}

