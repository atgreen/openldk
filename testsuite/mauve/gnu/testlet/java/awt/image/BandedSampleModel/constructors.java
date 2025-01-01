// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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

package gnu.testlet.java.awt.image.BandedSampleModel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.image.BandedSampleModel;
import java.awt.image.DataBuffer;

/**
 * Some checks for the constructors in the {@link BandedSampleModel} class.
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
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int)");
    
    BandedSampleModel m = new BandedSampleModel(DataBuffer.TYPE_SHORT, 10, 20, 2);
    harness.check(m.getDataType(), DataBuffer.TYPE_SHORT);
    harness.check(m.getWidth(), 10);
    harness.check(m.getHeight(), 20);
    harness.check(m.getNumBands(), 2);
    harness.check(m.getNumDataElements(), 2);
    harness.check(m.getScanlineStride(), 10);
    harness.check(m.getPixelStride(), 1);
    int[] bankIndices = m.getBankIndices();
    harness.check(bankIndices[0], 0);
    harness.check(bankIndices[1], 1);
   
    // check bad type
    boolean pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_UNDEFINED, 10, 20, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check zero width 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 0, 20, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // check zero height 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 10, 0, 2);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // check zero bands 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 10, 20, 0);
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(int, int, int, int, int[], int[])");
    
    BandedSampleModel m = new BandedSampleModel(DataBuffer.TYPE_SHORT, 10, 20, 10, new int[] {3, 2, 1}, new int[] {0, 0, 0});
    harness.check(m.getDataType(), DataBuffer.TYPE_SHORT);
    harness.check(m.getWidth(), 10);
    harness.check(m.getHeight(), 20);
    harness.check(m.getScanlineStride(), 10);
    harness.check(m.getPixelStride(), 1);
    harness.check(m.getNumBands(), 3);
    harness.check(m.getBankIndices()[0], 3);
    harness.check(m.getBankIndices()[1], 2);
    harness.check(m.getBankIndices()[2], 1);
    harness.check(m.getBandOffsets()[0], 0);
    harness.check(m.getBandOffsets()[1], 0);
    harness.check(m.getBandOffsets()[2], 0);
    
    // check bad type
    boolean pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_UNDEFINED, 10, 20, 10, new int[] {3, 2, 1}, new int[] {0, 0, 0});
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // check zero width 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 0, 20, 10, new int[] {3, 2, 1}, new int[] {0, 0, 0});
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);

    // check zero height 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 10, 0, 10, new int[] {3, 2, 1}, new int[] {0, 0, 0});
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
        
    // check null indices 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 10, 20, 10, null, new int[] {0, 0, 0});
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check null offsets 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 10, 20, 10, new int[] {3, 2, 1}, null);
    }
    catch (NullPointerException e)
    {
      pass = true;   
    }
    harness.check(pass);
    
    // check number of bands (inferred from array lengths) conflicting 
    pass = false;
    try
    {
      m = new BandedSampleModel(DataBuffer.TYPE_INT, 10, 20, 0, new int[] {2, 1}, new int[] {0, 0, 0});
    }
    catch (IllegalArgumentException e)
    {
      pass = true;   
    }
    harness.check(pass);
  }
}

