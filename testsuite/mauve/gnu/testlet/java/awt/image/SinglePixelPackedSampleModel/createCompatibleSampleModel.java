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

package gnu.testlet.java.awt.image.SinglePixelPackedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.DataBuffer;
import java.awt.image.SinglePixelPackedSampleModel;
import java.util.Arrays;

/**
 * Some checks for the <code>createCompatibleSampleModel()</code> method in the 
 * {@link SinglePixelPackedSampleModel} class.
 */
public class createCompatibleSampleModel implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 1, 2, new int[] { 224, 28, 3 }
    );
    SinglePixelPackedSampleModel m2 = (SinglePixelPackedSampleModel) m1.createCompatibleSampleModel(5, 10);
    harness.check(m2.getDataType(), DataBuffer.TYPE_BYTE);          // check 1
    harness.check(m2.getWidth(), 5);                                // check 2
    harness.check(m2.getHeight(), 10);                              // check 3
    harness.check(m2.getNumBands(), 3);                             // check 4
    harness.check(Arrays.equals(m1.getBitMasks(), m2.getBitMasks())); // check 5
 
    // if 'w' is <= 0 there should be an IllegalArgumentException
    try
    {
      /* SampleModel m3 = */ m1.createCompatibleSampleModel(0, 10);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
 
    // if 'h' is <= 0 there should be an IllegalArgumentException
    try
    {
      /* SampleModel m4 = */ m1.createCompatibleSampleModel(5, 0);
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  }

}

