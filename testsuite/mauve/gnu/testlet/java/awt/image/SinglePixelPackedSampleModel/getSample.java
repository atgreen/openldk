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
 * Some checks for the <code>getSample()</code> method in the 
 * {@link SinglePixelPackedSampleModel} class.
 */
public class getSample implements Testlet 
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
    int sample = m1.getSample(1, 1, 1, db);
    harness.check(sample, 0x0D);
 
    // check regular fetch with negative x
    try
    {
      sample = m1.getSample(-2, 0, 0, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      harness.check(true);
    }
 
    // check regular fetch with negative y
    try
    {
      sample = m1.getSample(0, -1, 0, db);
      harness.check(false);
    }
    catch (ArrayIndexOutOfBoundsException e) 
    {
      harness.check(true);
    }
  
    // check null data buffer
    try
    {
      sample = m1.getSample(0, 0, 0, null);
      harness.check(false);
    }
    catch (NullPointerException e) 
    {
      harness.check(true);
    }
  }
}

