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
import java.awt.image.DataBufferByte;
import java.awt.image.SinglePixelPackedSampleModel;

/**
 * Some checks for the <code>getSamples()</code> method in the 
 * {@link SinglePixelPackedSampleModel} class.
 */
public class getSamples implements Testlet 
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)      
  {
    SinglePixelPackedSampleModel m1 = new SinglePixelPackedSampleModel(
      DataBuffer.TYPE_BYTE, 2, 2, new int[] { 0xF0, 0x0F }
    );
    DataBufferByte db = new DataBufferByte(new byte[] { (byte) 0x12, (byte) 0x34, (byte) 0xAB, (byte) 0xCD }, 4);

    // check regular fetch
    int[] samples = m1.getSamples(0, 0, 1, 2, 1, (int[]) null, db);
    harness.check(samples[0], 0x02);  // 1
    harness.check(samples[1], 0x0B);  // 2

    // check regular fetch with negative x
    try  
    {
      samples = m1.getSamples(-1, 0, 1, 1, 1, (int[]) null, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)   
    {
      harness.check(true);
    }

    // check regular fetch with negative y
    try
    {
      samples = m1.getSamples(0, -1, 1, 1, 1, (int[]) null, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      harness.check(true);
    }

    // check regular fetch with w < 0
    try
    {
      samples = m1.getSamples(0, 0, -1, 1, 1, (int[]) null, db);
      harness.check(false);
    }
    catch (NegativeArraySizeException e)
    {
      harness.check(true);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      harness.check(true);
    }

    // check regular fetch with h < 0
    try
    {
      samples = m1.getSamples(0, 0, 1, -1, 1, (int[]) null, db);
      harness.check(false);
    }
    catch (NegativeArraySizeException e)
    {
      harness.check(true);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      harness.check(true);
    }

    // check regular fetch with band < 0
    try
    {
      samples = m1.getSamples(0, 0, 1, 1, -1, (int[]) null, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }

    // check regular fetch with presupplied array
    int[] samplesIn = new int[2];
    int[] samplesOut = m1.getSamples(1, 0, 1, 2, 0, samplesIn, db);
    harness.check(samplesIn == samplesOut);
    harness.check(samplesOut[0], 0x03);
    harness.check(samplesOut[1], 0x0C);

    // check regular fetch with presupplied array too short
    int[] samplesIn2 = new int[1];
    try 
    {
      /* int[] samplesOut2 = */ m1.getSamples(1, 1, 1, 2, 1, samplesIn2, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e)
    {
      harness.check(true);
    }

    // check null data buffer
    try
    {
      samples = m1.getSamples(0, 0, 1, 1, 1, (int[]) null, null);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
  }
}

