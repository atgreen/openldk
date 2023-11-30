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

/**
 * Some checks for the constructors in the {@link SinglePixelPackedSampleModel} class.
 */
public class constructors implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }

  private void testConstructor1(TestHarness harness) 
  {
    harness.checkPoint("(int, int, int, int[])");
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 1, 2, new int[] { 224, 28, 3 }
    );
    harness.check(m1.getDataType(), DataBuffer.TYPE_BYTE);       // check 1
    harness.check(m1.getWidth(), 1);                             // check 2
    harness.check(m1.getHeight(), 2);                            // check 3
    harness.check(m1.getNumBands(), 3);                          // check 4
    
    // unsupported data type should throw an IllegalArgumentException
    try                                                          // check 5
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_DOUBLE, 1, 2, new int[] { 224, 28, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // if 'w' is <= 0 there should be an IllegalArgumentException
    try                                                          // check 6
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 0, 2, new int[] { 224, 28, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // if 'h' is <= 0 there should be an IllegalArgumentException
    try                                                          // check 7
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 1, 0, new int[] { 224, 28, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // if mask array is empty there should be an IllegalArgumentException
    try                                                          // check 8
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 1, 2, new int[] { }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    // if mask array contains a non-contiguous mask there should be an IllegalArgumentException
    try
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 1, 2, new int[] { 224, 27, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
  }

  private void testConstructor2(TestHarness harness)   
  {
    harness.checkPoint("(int, int, int, int, int[])");
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 1, 2, 3, new int[] { 224, 28, 3 }
    );
    harness.check(m1.getDataType(), DataBuffer.TYPE_BYTE);       // check 1
    harness.check(m1.getWidth(), 1);                             // check 2
    harness.check(m1.getHeight(), 2);                            // check 3
    harness.check(m1.getNumBands(), 3);                          // check 4
    
    // unsupported data type should throw an IllegalArgumentException
    try
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_DOUBLE, 1, 2, 3, new int[] { 224, 28, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // if 'w' is <= 0 there should be an IllegalArgumentException
    try
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 0, 2, 3, new int[] { 224, 28, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // if 'h' is <= 0 there should be an IllegalArgumentException
    try
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 1, 0, 3, new int[] { 224, 28, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }
    
    // if mask array is empty there should be an IllegalArgumentException
    try
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 1, 2, 3, new int[] { }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

    // if mask array contains a non-contiguous mask there should be an IllegalArgumentException
    try
    {
      /* SampleModel m2 = */ new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_BYTE, 1, 2, new int[] { 224, 27, 3 }
      );
      harness.check(false);
    }
    catch (IllegalArgumentException e) 
    {
      harness.check(true);
    }

  }

}

